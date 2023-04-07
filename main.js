const { app, BrowserWindow, ipcMain } = require("electron");
const path = require("path");
const fs = require("fs/promises");

// TODO: Make this configurable
const dataPath = path.join(app.getPath("home"), ".tasks");
console.log("Using data path:", dataPath);

const createWindow = () => {
  const mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    autoHideMenuBar: true,
    webPreferences: {
      nodeIntegration: false,
      preload: path.join(__dirname, "preload.js"),
    },
  });
  mainWindow.loadFile("www/index.html");

  // Open the DevTools.
  // mainWindow.webContents.openDevTools()
};

app.whenReady().then(() => {
  createWindow();

  app.on("activate", () => {
    // On macOS it's common to re-create a window in the app when the
    // dock icon is clicked and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on("window-all-closed", () => {
  if (process.platform !== "darwin") app.quit();
});

ipcMain.handle("read-file", (event, fileName) => {
  const fullPath = path.join(dataPath, fileName);
  return fs.readFile(fullPath, { encoding: "utf-8" });
});

ipcMain.handle("write-file", async (event, fileName, data) => {
  const fullPath = path.join(dataPath, fileName);
  await fs.mkdir(dataPath, { recursive: true });
  await fs.writeFile(fullPath, data, { encoding: "utf-8" });
});
