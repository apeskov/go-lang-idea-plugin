/*
 * Copyright 2013-2015 Sergey Ignatov, Alexander Zolotov, Florin Patan
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

package com.goide.project.migration;

import com.goide.GoConstants;
import com.goide.project.GoApplicationLibrariesService;
import com.goide.project.GoProjectLibrariesService;
import com.goide.sdk.GoSdkType;
import com.intellij.conversion.*;
import com.intellij.ide.impl.convert.JDomConvertingUtil;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.impl.ProjectRootManagerImpl;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.SystemProperties;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializer;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.serialization.JDomSerializationUtil;

import java.io.File;
import java.io.IOException;
import java.util.Collection;

public class GoProjectModelConverterProvider extends ConverterProvider {

  public static final String PROJECT_ROOT_MANAGER = "ProjectRootManager";

  protected GoProjectModelConverterProvider() {
    super("go-project-model");
  }

  @NotNull
  @Override
  public String getConversionDescription() {
    return "Go project model has been changed so project and its modules need to be updated";
  }

  @NotNull
  @Override
  public ProjectConverter createConverter(@NotNull final ConversionContext context) {
    return new ProjectConverter() {
      private final Collection<File> additionalCreatedFiles = ContainerUtil.newArrayList();
      private final Collection<File> additionalAffectedFiles = ContainerUtil.newArrayList();

      @Nullable
      @Override
      public ConversionProcessor<ProjectSettings> createProjectFileConverter() {
        return new ProjectFileConverter();
      }

      @Override
      public boolean isConversionNeeded() {
        Element component = getProjectRootManager(context);
        return component != null && isGoSdkType(component.getAttributeValue(ProjectRootManagerImpl.PROJECT_JDK_TYPE_ATTR));
      }

      @Override
      public void preProcessingFinished() throws CannotConvertException {
        Element component = getProjectRootManager(context);
        if (component != null) {
          try {
            File miscFile = miscFile(context);
            updateSdkType(miscFile, component);
            additionalAffectedFiles.add(miscFile);

            File oldGoSettings = new File(context.getSettingsBaseDir(), "go_settings.xml");
            if (oldGoSettings.exists()) {
              Element oldGoSettingsRoot = JDomConvertingUtil.loadDocument(oldGoSettings).getRootElement();
              if (isAttachProjectDirToLibraries(oldGoSettingsRoot)) {
                File librariesConfigFile = new File(context.getSettingsBaseDir(), GoConstants.GO_LIBRARIES_CONFIG_FILE);
                if (librariesConfigFile.exists()) {
                  //noinspection ResultOfMethodCallIgnored
                  librariesConfigFile.delete();
                  additionalAffectedFiles.add(librariesConfigFile);
                }
                else {
                  additionalCreatedFiles.add(librariesConfigFile);
                }
                FileUtil.writeToFile(librariesConfigFile, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<project></project>");
                addProjectDirToLibraries(JDomConvertingUtil.loadDocument(librariesConfigFile).getRootElement());
              }
            }
            //noinspection ResultOfMethodCallIgnored
            oldGoSettings.delete();
          }
          catch (IOException e) {
            throw new CannotConvertException("Cannot convert go project", e);
          }
        }
        convertSdks();
      }

      @Override
      public Collection<File> getAdditionalAffectedFiles() {
        return additionalAffectedFiles;
      }

      @Override
      public Collection<File> getCreatedFiles() {
        return additionalCreatedFiles;
      }
    };
  }

  private static class ProjectFileConverter extends ConversionProcessor<ProjectSettings> {
    @Override
    public boolean isConversionNeeded(ProjectSettings settings) {
      Element projectRootManager = getProjectRootManager(settings.getRootElement());
      return projectRootManager != null && isGoSdkType(projectRootManager.getAttributeValue(ProjectRootManagerImpl.PROJECT_JDK_TYPE_ATTR));
    }

    @Override
    public void process(ProjectSettings settings) throws CannotConvertException {
      Element projectRootManager = getProjectRootManager(settings.getRootElement());
      if (projectRootManager != null) {
        updateSdkType(settings.getFile(), projectRootManager);
      }
      if (isAttachProjectDirToLibraries(settings.getRootElement())) {
        addProjectDirToLibraries(settings.getRootElement());
      }
      convertSdks();
    }
  }

  private static Element getProjectRootManager(ConversionContext context) {
    File miscFile = miscFile(context);
    try {
      if (miscFile.exists()) {
        return getProjectRootManager(JDomConvertingUtil.loadDocument(miscFile).getRootElement());
      }
    }
    catch (CannotConvertException e) {
      return null;
    }
    return null;
  }

  private static Element getProjectRootManager(Element rootElement) {
    return rootElement != null ? JDomSerializationUtil.findComponent(rootElement, PROJECT_ROOT_MANAGER) : null;
  }

  private static void updateSdkType(File file, Element projectRootManager) throws CannotConvertException {
    projectRootManager.setAttribute(ProjectRootManagerImpl.PROJECT_JDK_TYPE_ATTR, "Go SDK");
    try {
      JDOMUtil.writeDocument(projectRootManager.getDocument(), file, SystemProperties.getLineSeparator());
    }
    catch (IOException e) {
      throw new CannotConvertException("Cannot save sdk type changing", e);
    }
  }

  @NotNull
  private static File miscFile(ConversionContext context) {
    return new File(context.getSettingsBaseDir(), "misc.xml");
  }

  private static void addProjectDirToLibraries(Element rootElement) {
    GoProjectLibrariesService librariesService = new GoProjectLibrariesService();
    librariesService.setLibraryRootUrls("file://$PROJECT_DIR$");
    Element componentElement = JDomSerializationUtil.findOrCreateComponentElement(rootElement, GoConstants.GO_LIBRARIES_SERVICE_NAME);
    XmlSerializer.serializeInto(librariesService, componentElement);
  }

  private static boolean isAttachProjectDirToLibraries(Element rootElement) {
    Element goProjectSettings = JDomSerializationUtil.findComponent(rootElement, "GoProjectSettings");
    if (goProjectSettings != null) {
      String prependGoPath = goProjectSettings.getAttributeValue("prependGoPath");
      goProjectSettings.detach();
      return "true".equalsIgnoreCase(prependGoPath);
    }
    return false;
  }

  private static boolean isGoSdkType(String sdkTypeName) {
    return "Google Go SDK".equals(sdkTypeName) || "Google Go App Engine SDK".equals(sdkTypeName);
  }

  private static void convertSdks() {
    ProjectJdkTable sdkTable = ProjectJdkTable.getInstance();
    Collection<String> globalGoPathUrls = ContainerUtil.newLinkedHashSet();
    Collection<Sdk> sdksToDelete = ContainerUtil.newArrayList();
    Collection<Sdk> sdksToAdd = ContainerUtil.newArrayList();
    GoSdkType sdkType = GoSdkType.getInstance();
    for (Sdk sdk : sdkTable.getAllJdks()) {
      String sdkTypeName = sdk.getSdkType().getName();
      if (isGoSdkType(sdkTypeName)) {
        sdksToDelete.add(sdk);
        String sdkHome = sdkType.adjustSelectedSdkHome(sdk.getHomePath());
        if (sdkType.isValidSdkHome(sdkHome)) {
          ProjectJdkImpl newSdk = new ProjectJdkImpl(sdk.getName(), sdkType, sdkHome, sdkType.getVersionString(sdkHome));
          sdkType.setupSdkPaths(newSdk);
          sdksToAdd.add(newSdk);

          for (String classesRoot : sdk.getRootProvider().getUrls(OrderRootType.CLASSES)) {
            if (!classesRoot.equals(sdk.getHomePath())) {
              globalGoPathUrls.add(classesRoot);
            }
          }
        }
      }
    }

    AccessToken l = WriteAction.start();
    try {
      for (Sdk sdk : sdksToDelete) {
        sdkTable.removeJdk(sdk);
      }
      for (Sdk sdk : sdksToAdd) {
        sdkTable.addJdk(sdk);
      }
      globalGoPathUrls.addAll(GoApplicationLibrariesService.getInstance().getLibraryRootUrls());
      GoApplicationLibrariesService.getInstance().setLibraryRootUrls(globalGoPathUrls);
    }
    finally {
      l.finish();
    }
  }
}