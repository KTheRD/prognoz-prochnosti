import { app, shell, BrowserWindow, ipcMain } from 'electron'
import { join } from 'path'
import { electronApp, optimizer, is } from '@electron-toolkit/utils'
import { IPty, spawn } from 'node-pty'
import prognozLin from '../../resources/generated/prognoz?asset&asarUnpack'
import prognozWin from '../../resources/generated/prognoz.exe?asset&asarUnpack'

const prognoz = process.platform === 'win32' ? prognozWin : prognozLin

let subProcess = null as null | IPty

async function startSubProcess(event: Electron.IpcMainInvokeEvent, parameters: number[]) {
  if (subProcess !== null) {
    subProcess.kill()
    await new Promise((resolve) => subProcess?.onExit(resolve))
    subProcess = null
  }
  subProcess = spawn(prognoz, [], {})
  parameters.forEach((e) => {
    if (subProcess === null) return
    subProcess.write(`${e}\n`)
  })
  subProcess.onData((data) => event.sender.send('data', data))
  subProcess.onExit(() => (subProcess = null))
}

function killSubProcess() {
  subProcess?.kill()
  subProcess = null
}

function writeToSubprocess(_: Electron.IpcMainInvokeEvent, data: number) {
  subProcess?.write(`${data}\n`)
}

function createWindow(): void {
  // Create the browser window.
  const mainWindow = new BrowserWindow({
    width: 900,
    height: 670,
    show: false,
    autoHideMenuBar: true,
    webPreferences: {
      preload: join(__dirname, '../preload/index.js'),
      sandbox: false
    }
  })

  mainWindow.on('ready-to-show', () => {
    mainWindow.show()
  })

  mainWindow.webContents.setWindowOpenHandler((details) => {
    shell.openExternal(details.url)
    return { action: 'deny' }
  })

  // HMR for renderer base on electron-vite cli.
  // Load the remote URL for development or the local html file for production.
  if (is.dev && process.env['ELECTRON_RENDERER_URL']) {
    mainWindow.loadURL(process.env['ELECTRON_RENDERER_URL'])
  } else {
    mainWindow.loadFile(join(__dirname, '../renderer/index.html'))
  }
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.whenReady().then(() => {
  // Set app user model id for windows
  electronApp.setAppUserModelId('com.electron')

  // Default open or close DevTools by F12 in development
  // and ignore CommandOrControl + R in production.
  // see https://github.com/alex8088/electron-toolkit/tree/master/packages/utils
  app.on('browser-window-created', (_, window) => {
    optimizer.watchWindowShortcuts(window)
  })

  ipcMain.on('startPrognozSubprocess', startSubProcess)
  ipcMain.on('killPrognozSubprocess', killSubProcess)
  ipcMain.on('writeToSubprocess', writeToSubprocess)

  createWindow()

  app.on('activate', function () {
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

// Quit when all windows are closed, except on macOS. There, it's common
// for applications and their menu bar to stay active until the user quits
// explicitly with Cmd + Q.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') {
    app.quit()
  }
})

// In this file you can include the rest of your app"s specific main process
// code. You can also put them in separate files and require them here.
