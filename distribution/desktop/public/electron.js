const electron = require('electron')
const path = require('path')
const fs = require('fs')
const execFile = require('child_process').execFile

let MAIN_WINDOW
let SYSTEM_TRAY

electron.app.on('certificate-error',
  (event, webContents, url, error, certificate, accept) => {
    if (url.startsWith('https://127.0.0.1')) {
      accept(true)
    } else {
      accept(false)
    }
  }
)

electron.app.on('ready', () => {
  const appDir = path.dirname(require.main.filename)
  const appImagePath = `${appDir}/TerminusDB-amd64.AppImage`
  const exePath = `${appDir}/windows/start_windows.bat`

  let binPath
  let binArgs
  if (fs.existsSync(appImagePath)) {
    binPath = appImagePath
    binArgs = ['serve']
  } else if (fs.existsSync(exePath)) {
    binPath = exePath
    binArgs = []
  }

  if (binPath) {
    console.log('PATH', binPath)
    let serverReady = false
    console.log('Starting TerminusDB')
    const child = execFile(binPath, binArgs)
    child.stderr.on('data', (data) => {
      if (!serverReady) {
        console.log('stderr: ' + data)
        if (data.toString().substring(2, 16) === 'Started server') {
          serverReady = true
          console.log('Terminusdb started')
          createWindow()
        }
      }
    })
  } else {
    console.log('TerminusDB not found in path')
    console.log('Please start TerminusDB')
    createWindow()
  }
})

function createWindow () {
  MAIN_WINDOW = new electron.BrowserWindow({
    width: 900,
    height: 680,
    nodeIntegration: false,
    webPreferences: {
      spellcheck: false
    },
    icon: path.join(__dirname, 'assets/icons/favicon.png')
  })

  MAIN_WINDOW.on('minimize', function (event) {
    event.preventDefault()
    MAIN_WINDOW.hide()
  })

  MAIN_WINDOW.on('close', function (event) {
    if (!electron.app.isQuiting) {
      event.preventDefault()
      MAIN_WINDOW.hide()
    }
    return false
  })

  MAIN_WINDOW.webContents.on('new-window', function (e, url) {
    e.preventDefault()
    electron.shell.openExternal(url)
  })

  MAIN_WINDOW.loadURL('https://127.0.0.1:6363/')

  const appMenu = new electron.Menu()

  appMenu.append(new electron.MenuItem({
    label: 'Reload (Ctrl+R)',
    accelerator: 'CommandOrControl+R',
    click () {
      MAIN_WINDOW.reload()
    }
  }))
  appMenu.append(new electron.MenuItem({
    label: 'Reload (F5)',
    accelerator: 'f5',
    click () {
      MAIN_WINDOW.reload()
    }
  }))
  appMenu.append(new electron.MenuItem({
    label: 'Back',
    accelerator: 'Alt+Left',
    click () {
      MAIN_WINDOW.webContents.goBack()
    }
  }))
  appMenu.append(new electron.MenuItem({
    label: 'Back',
    accelerator: 'Alt+Right',
    click () {
      MAIN_WINDOW.webContents.goForward()
    }
  }))
  appMenu.append(new electron.MenuItem({
    accelerator: 'CommandOrControl+B',
    label: 'Open in Browser',
    click () {
      electron.shell.openExternal('https://127.0.0.1:6363')
    }
  }))

  electron.Menu.setApplicationMenu(appMenu)
  MAIN_WINDOW.setMenuBarVisibility(false)

  const trayMenu = new electron.Menu()

  trayMenu.append(new electron.MenuItem({
    label: 'TerminusDB',
    click () {
      MAIN_WINDOW.show()
    }
  }))
  trayMenu.append(new electron.MenuItem({ type: 'separator' }))
  trayMenu.append(new electron.MenuItem({
    label: 'Hide',
    click () {
      MAIN_WINDOW.hide()
    }
  }))
  trayMenu.append(new electron.MenuItem({
    label: 'Reload',
    click () {
      MAIN_WINDOW.show()
      MAIN_WINDOW.reload()
    }
  }))
  trayMenu.append(new electron.MenuItem({
    label: 'Quit',
    click () {
      electron.app.isQuiting = true
      electron.app.quit()
    }
  }))

  SYSTEM_TRAY = new electron.Tray(path.join(__dirname,
    'assets/icons/favicon.png'))

  if (process.platform === 'win32') {
    SYSTEM_TRAY.on('click', SYSTEM_TRAY.popUpContextMenu)
  }

  SYSTEM_TRAY.setContextMenu(trayMenu)
  SYSTEM_TRAY.setToolTip('TerminusDB')
}

