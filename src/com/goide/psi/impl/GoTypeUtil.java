/*
 * Copyright 2013-2016 Sergey Ignatov, Alexander Zolotov, Florin Patan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.goide.psi.impl;

import com.goide.GoConstants;
import com.goide.psi.*;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.SyntaxTraverser;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.ThreeState;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.List;

public class GoTypeUtil {
  /**
   * https://golang.org/ref/spec#For_statements
   * The expression on the right in the "range" clause is called the range expression,
   * which may be an array, pointer to an array, slice, string, map, or channel permitting receive operations.
   */
  public static boolean isIterable(@Nullable GoType type) {
    type = type != null ? type.getUnderlyingType() : null;
    return type instanceof GoArrayOrSliceType ||
           type instanceof GoPointerType && isArray(((GoPointerType)type).getType()) ||
           type instanceof GoMapType ||
           type instanceof GoChannelType ||
           isString(type);
  }

  private static boolean isArray(@Nullable GoType type) {
    type = type != null ? type.getUnderlyingType() : null;
    return type instanceof GoArrayOrSliceType && ((GoArrayOrSliceType)type).getExpression() != null;
  }

  public static boolean isString(@Nullable GoType type) {
    return isBuiltinType(type, "string");
  }

  public static boolean isBoolean(@Nullable GoType type) {
    return isBuiltinType(type, "bool");
  }

  private static boolean isBuiltinType(@Nullable GoType type, @Nullable String builtinTypeName) {
    if (builtinTypeName == null) return false;
    type = type != null ? type.getUnderlyingType() : null;
    return type != null && type.textMatches(builtinTypeName) && GoPsiImplUtil.builtin(type);
  }

  @NotNull
  public static List<GoType> getExpectedTypes(@NotNull GoExpression expression) {
    PsiElement parent = expression.getParent();
    if (parent == null) return Collections.emptyList();
    if (parent instanceof GoAssignmentStatement) {
      return getExpectedTypesFromAssignmentStatement(expression, (GoAssignmentStatement)parent);
    }
    if (parent instanceof GoRangeClause) {
      return Collections.singletonList(getGoType(null, parent));
    }
    if (parent instanceof GoRecvStatement) {
      return getExpectedTypesFromRecvStatement((GoRecvStatement)parent);
    }
    if (parent instanceof GoVarSpec) {
      return getExpectedTypesFromVarSpec(expression, (GoVarSpec)parent);
    }
    if (parent instanceof GoArgumentList) {
      return getExpectedTypesFromArgumentList(expression, (GoArgumentList)parent);
    }
    if (parent instanceof GoUnaryExpr) {
      GoUnaryExpr unaryExpr = (GoUnaryExpr)parent;
      if (unaryExpr.getSendChannel() != null) {
        GoType type = ContainerUtil.getFirstItem(getExpectedTypes(unaryExpr));
        GoType chanType = GoElementFactory.createType(parent.getProject(), "chan " + getInterfaceIfNull(type, parent).getText());
        return Collections.singletonList(chanType);
      }
      else {
        return Collections.singletonList(getGoType(null, parent));
      }
    }
    if (parent instanceof GoSendStatement || parent instanceof GoLeftHandExprList && parent.getParent() instanceof GoSendStatement) {
      GoSendStatement sendStatement = (GoSendStatement)(parent instanceof GoSendStatement ? parent : parent.getParent());
      return getExpectedTypesFromGoSendStatement(expression, sendStatement);
    }
    if (parent instanceof GoExprCaseClause) {
      return getExpectedTypesFromExprCaseClause((GoExprCaseClause)parent);
    }
    return Collections.emptyList();
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromExprCaseClause(@NotNull GoExprCaseClause exprCaseClause) {
    GoExprSwitchStatement switchStatement = PsiTreeUtil.getParentOfType(exprCaseClause, GoExprSwitchStatement.class);
    assert switchStatement != null;

    GoExpression switchExpr = switchStatement.getExpression();
    if (switchExpr != null) {
      return Collections.singletonList(getGoType(switchExpr, exprCaseClause));
    }

    GoStatement statement = switchStatement.getStatement();
    if (statement == null) {
      return Collections.singletonList(getInterfaceIfNull(GoPsiImplUtil.getBuiltinType("bool", exprCaseClause), exprCaseClause));
    }
    
    GoLeftHandExprList leftHandExprList = statement instanceof GoSimpleStatement ? ((GoSimpleStatement)statement).getLeftHandExprList() : null;
    GoExpression expr = leftHandExprList != null ? ContainerUtil.getFirstItem(leftHandExprList.getExpressionList()) : null;
    return Collections.singletonList(getGoType(expr, exprCaseClause));
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromGoSendStatement(@NotNull GoExpression expression, @NotNull GoSendStatement statement) {
    GoLeftHandExprList leftHandExprList = statement.getLeftHandExprList();
    GoExpression channel = ContainerUtil.getFirstItem(leftHandExprList != null ? leftHandExprList.getExpressionList() : statement.getExpressionList());
    GoExpression sendExpr = statement.getSendExpression();
    assert channel != null;
    if (expression.isEquivalentTo(sendExpr)) {
      GoType chanType = channel.getGoType(null);
      if (chanType instanceof GoChannelType) {
        return Collections.singletonList(getInterfaceIfNull(((GoChannelType)chanType).getType(), statement));
      }
    }
    if (expression.isEquivalentTo(channel)) {
      GoType type = sendExpr != null ? sendExpr.getGoType(null) : null;
      GoType chanType = GoElementFactory.createType(statement.getProject(), "chan " + getInterfaceIfNull(type, statement).getText());
      return Collections.singletonList(chanType);
    }
    return Collections.singletonList(getInterfaceIfNull(null, statement));
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromArgumentList(@NotNull GoExpression expression, @NotNull GoArgumentList argumentList) {
    PsiElement parentOfParent = argumentList.getParent();
    assert parentOfParent instanceof GoCallExpr;
    PsiReference reference = ((GoCallExpr)parentOfParent).getExpression().getReference();
    if (reference != null) {
      PsiElement resolve = reference.resolve();
      if (resolve instanceof GoFunctionOrMethodDeclaration) {
        GoSignature signature = ((GoFunctionOrMethodDeclaration)resolve).getSignature();
        if (signature != null) {
          List<GoExpression> exprList = argumentList.getExpressionList();
          List<GoParameterDeclaration> paramsList = signature.getParameters().getParameterDeclarationList();
          if (exprList.size() == 1) {
            List<GoType> typeList = ContainerUtil.newSmartList();
            for (GoParameterDeclaration parameterDecl : paramsList) {
              for (GoParamDefinition parameter : parameterDecl.getParamDefinitionList()) {
                typeList.add(getGoType(parameter, argumentList));
              }
              if (parameterDecl.getParamDefinitionList().isEmpty()) {
                typeList.add(getInterfaceIfNull(parameterDecl.getType(), argumentList));
              }
            }
            List<GoType> result = ContainerUtil.newSmartList(createGoTypeListOrGoType(typeList, argumentList));
            if (paramsList.size() > 1) {
              assert paramsList.get(0) != null;
              result.add(getInterfaceIfNull(paramsList.get(0).getType(), argumentList));
            }
            return result;
          }
          else {
            int position = exprList.indexOf(expression);
            if (position >= 0) {
              int i = 0;
              for (GoParameterDeclaration parameterDecl : paramsList) {
                int paramDeclSize = Math.max(1, parameterDecl.getParamDefinitionList().size());
                if (i + paramDeclSize > position) {
                  return Collections.singletonList(getInterfaceIfNull(parameterDecl.getType(), argumentList));
                }
                i += paramDeclSize;
              }
            }
          }
        }
      }
    }
    return Collections.singletonList(getInterfaceIfNull(null, argumentList));
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromRecvStatement(@NotNull GoRecvStatement recvStatement) {
    List<GoType> typeList = ContainerUtil.newSmartList();
    for (GoExpression expr : recvStatement.getLeftExpressionsList()) {
      typeList.add(getGoType(expr, recvStatement));
    }
    return Collections.singletonList(createGoTypeListOrGoType(typeList, recvStatement));
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromVarSpec(@NotNull GoExpression expression, @NotNull GoVarSpec varSpec) {
    List<GoType> result = ContainerUtil.newSmartList();
    GoType type = getInterfaceIfNull(varSpec.getType(), varSpec);
    if (varSpec.getRightExpressionsList().size() == 1) {
      List<GoType> typeList = ContainerUtil.newSmartList();
      int defListSize = varSpec.getVarDefinitionList().size();
      for (int i = 0; i < defListSize; i++) {
        typeList.add(type);
      }
      result.add(createGoTypeListOrGoType(typeList, expression));
      if (defListSize > 1) {
        result.add(getInterfaceIfNull(type, varSpec));
      }
      return result;
    }
    result.add(type);
    return result;
  }

  @NotNull
  private static List<GoType> getExpectedTypesFromAssignmentStatement(@NotNull GoExpression expression,
                                                                      @NotNull GoAssignmentStatement assignment) {
    List<GoExpression> leftExpressions = assignment.getLeftHandExprList().getExpressionList();
    if (assignment.getExpressionList().size() == 1) {
      List<GoType> typeList = ContainerUtil.newSmartList();
      for (GoExpression expr : leftExpressions) {
        GoType type = expr.getGoType(null);
        typeList.add(type);
      }
      List<GoType> result = ContainerUtil.newSmartList(createGoTypeListOrGoType(typeList, expression));
      if (leftExpressions.size() > 1) {
        result.add(getGoType(leftExpressions.get(0), assignment));
      }
      return result;
    }
    
    int position = assignment.getExpressionList().indexOf(expression);
    GoType leftExpression = leftExpressions.size() > position ? leftExpressions.get(position).getGoType(null) : null;
    return Collections.singletonList(getInterfaceIfNull(leftExpression, assignment));
  }

  @NotNull
  private static GoType createGoTypeListOrGoType(@NotNull List<GoType> types, @NotNull PsiElement context) {
    if (types.size() < 2) {
      return getInterfaceIfNull(ContainerUtil.getFirstItem(types), context);
    }
    return GoElementFactory.createTypeList(context.getProject(), StringUtil.join(types, new Function<GoType, String>() {
      @Override
      public String fun(GoType type) {
        return type == null ? GoConstants.INTERFACE_TYPE : type.getText();
      }
    }, ", "));
  }

  @NotNull
  private static GoType getInterfaceIfNull(@Nullable GoType type, @NotNull PsiElement context) {
    return type == null ? GoElementFactory.createType(context.getProject(), GoConstants.INTERFACE_TYPE) : type;
  }

  @NotNull
  private static GoType getGoType(@Nullable GoTypeOwner element, @NotNull PsiElement context) {
    return getInterfaceIfNull(element != null ? element.getGoType(null) : null, context);
  }

  @NotNull
  public static ThreeState isAssignable(@NotNull GoType left, @Nullable GoType right) {
    if (right == null) return ThreeState.NO;
    if (left == right || left.equals(right)) return ThreeState.YES;

    if (identical(left, right)) return ThreeState.YES;

    // todo: and at least one of V or T is not a named type
    if (identical(left.getUnderlyingType(), right.getUnderlyingType())) {
      return ThreeState.YES; // x's type V and T have identical underlying types and at least one of V or T is not a named type.
    }
    
    /*
    Assignability
    
    A value x is assignable to a variable of type T ("x is assignable to T") in any of these cases:
    
    x's type is identical to T.
    x's type V and T have identical underlying types and at least one of V or T is not a named type.
    T is an interface type and x implements T.
    x is a bidirectional channel value, T is a channel type, x's type V and T have identical element types, and at least one of V or T is not a named type.
    x is an untyped constant representable by a value of type T.
    */

    return ThreeState.UNSURE;
  }

  public static boolean identical(@Nullable GoType left, @Nullable GoType right) {
    if (left == null || right == null) return false;
    if (left instanceof GoSpecType) {
      return right instanceof GoSpecType && left.isEquivalentTo(right);
    }
    if (left instanceof GoArrayOrSliceType) {
      // todo: length
      return right instanceof GoArrayOrSliceType && identical(((GoArrayOrSliceType)left).getType(), ((GoArrayOrSliceType)right).getType());
    }
    if (left instanceof GoStructType) {
      // todo
      // Two struct types are identical if they have the same sequence of fields, and if corresponding fields have the same names, and identical types, and identical tags. Two anonymous fields are considered to have the same name. Lower-case field names from different packages are always different.
      return right instanceof GoStructType && identicalStructs((GoStructType)left, (GoStructType)right);
    }
    if (left instanceof GoPointerType) {
      return right instanceof GoPointerType && identical(((GoPointerType)left).getType(), ((GoPointerType)right).getType());
    }
    if (left instanceof GoFunctionType) {
      // todo
      // Two function types are identical if they have the same number of parameters and result values, corresponding parameter and result types are identical, and either both functions are variadic or neither is. Parameter and result names are not required to match.
      return right instanceof GoFunctionType;
    }
    if (left instanceof GoInterfaceType) {
      // todo
      // Two interface types are identical if they have the same set of methods with the same names and identical function types.
      // Lower-case method names from different packages are always different. The order of the methods is irrelevant.
      // todo: T is an interface type and x implements T
      return right instanceof GoInterfaceType;
    }
    if (left instanceof GoMapType) {
      return right instanceof GoMapType
             && identical(((GoMapType)left).getKeyType(), ((GoMapType)right).getKeyType())
             && identical(((GoMapType)left).getValueType(), ((GoMapType)right).getValueType());
    }
    if (left instanceof GoChannelType) {
      // todo: and the same direction
      return right instanceof GoChannelType && identical(((GoChannelType)left).getType(), ((GoChannelType)right).getType());
    }
    if (Comparing.equal(left.getUnderlyingType(), right.getUnderlyingType())) return true;

    // GoReceiverType, GoCType, GoTypeList 

    return false;
  }

  private static boolean identicalStructs(@NotNull GoStructType left, @NotNull GoStructType right) {
    List<GoNamedElement> l = SyntaxTraverser.psiTraverser(left).filter(GoNamedElement.class).toList();
    List<GoNamedElement> r = SyntaxTraverser.psiTraverser(right).filter(GoNamedElement.class).toList();
    if (l.size() != r.size()) return false;
    List<String> lNames = ContainerUtil.map(l, PsiNamedElement::getName);
    List<String> rNames = ContainerUtil.map(r, PsiNamedElement::getName);
    if (!ContainerUtil.equalsIdentity(lNames, rNames)) return false;

    for (int i = 0; i < l.size(); i++) {
      GoNamedElement f = l.get(i);
      GoNamedElement s = r.get(i);
      if (f instanceof GoFieldDefinition) {
        if (!(s instanceof GoFieldDefinition)) return false;
        if (!identical(f.getGoType(null), s.getGoType(null))) return false;
      }
      if (f instanceof GoAnonymousFieldDefinition) {
        if (!(s instanceof GoAnonymousFieldDefinition)) return false;
        if (!identical(((GoAnonymousFieldDefinition)f).getTypeReferenceExpression().resolveType(), 
                       ((GoAnonymousFieldDefinition)s).getTypeReferenceExpression().resolveType())) return false;
      }
    }
    return true;
  }
}
