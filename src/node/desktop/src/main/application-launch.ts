/*
 * application-launch.ts
 *
 * Copyright (C) 2022 by Posit Software, PBC
 *
 * Unless you have received this program directly from Posit Software pursuant
 * to the terms of a commercial license agreement with Posit Software, then
 * this program is licensed to you under the terms of version 3 of the
 * GNU Affero General Public License. This program is distributed WITHOUT
 * ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
 * AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
 *
 */

import path from 'path';
import { spawn } from 'child_process';
import { existsSync, readdirSync } from 'fs';
import { setenv, unsetenv } from '../core/environment';
import { kRStudioInitialProject, kRStudioInitialWorkingDir } from '../core/r-user-data';
import { MainWindow } from './main-window';
import { app } from 'electron';
import { chdir, cwd } from 'process';
import { logger } from '../core/logger';

export interface LaunchRStudioOptions {
  projectFilePath?: string;
  workingDirectory?: string;
}

export function resolveProjectFile(projectDir: string): string {
  // check that the project directory exists
  if (!existsSync(projectDir)) {
    return '';
  }

  // list files in the directory, looking for a .Rproj file
  const files = readdirSync(projectDir, { encoding: 'utf-8' });
  for (const file of files) {
    const ext = path.extname(file).toLowerCase();
    if (ext === '.rproj') {
      return path.join(projectDir, file);
    }
  }

  // nothing found
  return '';
}

/**
 * Not clear yet if we'll need this class from the Qt implementation, but keeping
 * it for now. If it ends up being useful, probably need to create an interface
 * describing behavior (for easier unit testing).
 */
export class ApplicationLaunch {
  mainWindow?: MainWindow;
  static init(): ApplicationLaunch {
    return new ApplicationLaunch();
  }

  setActivationWindow(window: MainWindow): void {
    this.mainWindow = window;
  }

  activateWindow(): void {
    // TODO - reimplement (if needed at all)
  }

  launchRStudio(options: LaunchRStudioOptions): void {
    // in devmode, we need to pass the directory path when launching the application;
    // for package builds, we have no such requirement
    const argv = app.isPackaged ? [] : [process.argv[1]];

    // resolve working directory
    const workingDir = options.workingDirectory ?? path.dirname(options.projectFilePath || '');
    setenv(kRStudioInitialWorkingDir, workingDir);

    // try using the requested working directory
    const currentWorkingDir = cwd();
    if (workingDir.length !== 0) {
      try {
        chdir(workingDir);
      } catch (err) {
        logger().logError(err);
      }
    }

    // resolve project file, if any
    const projectFile = options.projectFilePath ?? resolveProjectFile(workingDir);
    if (existsSync(projectFile)) {
      setenv(kRStudioInitialProject, projectFile);
    }

    // run it
    const childProcess = spawn(process.execPath, argv, {
      detached: true,
      stdio: 'ignore', // don't reuse the stdio from parent
    });
    childProcess.unref();

    // restore working directory
    try {
      chdir(currentWorkingDir);
    } catch (err) {
      logger().logError(err);
    }

    // restore environment variables
    unsetenv(kRStudioInitialProject);
    unsetenv(kRStudioInitialWorkingDir);
  }
}
