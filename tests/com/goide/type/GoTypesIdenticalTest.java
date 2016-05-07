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

package com.goide.type;

import com.goide.GoCodeInsightFixtureTestCase;
import com.goide.psi.GoFile;
import com.goide.psi.GoType;
import com.goide.psi.GoVarDefinition;
import com.goide.psi.impl.GoTypeUtil;
import com.intellij.testFramework.EdtTestUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.TestRunnerUtil;
import com.intellij.util.ThrowableRunnable;
import org.jetbrains.annotations.NotNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@RunWith(Parameterized.class)
public class GoTypesIdenticalTest extends GoCodeInsightFixtureTestCase {
  @Parameterized.Parameters
  public static Collection<Object[]> data() {
    return Arrays.asList(new Object[][]{
      {"[]string", "[]string", true},
      {"string", "[]string", false},
      {"map[int]string", "map[int]string", true},
      {"map[int]string", "map[int]int", false},
      {"chan<- float64", "chan<- float64", true},
      {"chan<- string", "chan<- float64", false},
      {"<-chan <-chan int", "<-chan <-chan int", true},
      {"<-chan <-chan int", "<-chan <-chan string", false},
      {"string", "string", true},
      {"int", "string", false},
      {"interface{}", "interface{}", true},
      {"struct{}", "struct{}", true},
      {"struct{}", "struct{a int}", false},
      {"struct{a int}", "struct{a int}", true},
      {"struct{a int}", "struct{b int}", false},
      {"struct{a int}", "struct{a string; b int}", false},
      {"struct{a int; int}", "struct{a int; int string}", false},
    });
  }

  private String left;
  private String right;
  private boolean ok;

  @SuppressWarnings("JUnitTestCaseWithNonTrivialConstructors")
  public GoTypesIdenticalTest(String left, String right, boolean ok) {
    this.left = left;
    this.right = right;
    this.ok = ok;
  }

  @Test
  public void test() throws Exception {
    safeEdt(new ThrowableRunnable<Throwable>() {
      @Override
      public void run() {
        doTest();
      }
    });
  }

  private void doTest() {
    myFixture.configureByText("a.go", "package main;var x " + left + ";var y " + right);
    List<GoVarDefinition> vars = ((GoFile)myFixture.getFile()).getVars();
    GoType left = vars.get(0).getGoType(null);
    GoType right = vars.get(1).getGoType(null);
    String leftText = left == null ? null : left.getText();
    String rightText = right == null ? null : right.getText();
    assertTrue(leftText + " should" + (!ok ? " not " : " ") + "equal " + rightText, ok == GoTypeUtil.identical(left, right));
  }

  @Before
  @Override
  public void setUp() throws Exception {
    safeEdt(new ThrowableRunnable<Throwable>() {
      @Override
      public void run() throws Throwable {
        GoTypesIdenticalTest.super.setUp();
      }
    });
  }

  @After
  @Override
  public void tearDown() throws Exception {
    safeEdt(new ThrowableRunnable<Throwable>() {
      @Override
      public void run() throws Throwable {
        GoTypesIdenticalTest.super.tearDown();
      }
    });
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return createMockProjectDescriptor();
  }

  private void safeEdt(@NotNull ThrowableRunnable<Throwable> r) throws Exception {
    if (runInDispatchThread()) {
      TestRunnerUtil.replaceIdeEventQueueSafely();
      EdtTestUtil.runInEdtAndWait(new ThrowableRunnable<Throwable>() {
        @Override
        public void run() throws Throwable {
          r.run();
        }
      });
    }
    else {
      try {
        r.run();
      }
      catch (Throwable t) {
        throw new RuntimeException(t);
      }
    }
  }
}
