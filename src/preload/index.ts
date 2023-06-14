import { contextBridge, ipcRenderer, IpcRendererEvent } from 'electron'

contextBridge.exposeInMainWorld('api', {
  startPrognozSubprocess: (parameters: number[]) =>
    ipcRenderer.send('startPrognozSubprocess', parameters),
  killPrognozSubprocess: () => ipcRenderer.send('kill'),
  onData: (callback: (e: IpcRendererEvent, ...args: any) => void) =>
    void ipcRenderer.on('data', callback),
  removeDataListener: () => void ipcRenderer.removeAllListeners('data'),
  sendToSubprocess: (data:number) => ipcRenderer.send('writeToSubprocess', data),
})
