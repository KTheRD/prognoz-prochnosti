import { ElectronAPI } from '@electron-toolkit/preload'
import { IpcRenderer } from 'electron'

declare global {
  interface Window {
    api: {
      startPrognozSubprocess: (parameters: number[]) => void
      killPrognozSubprocess: () => void
      onData: (callback: (e: IpcRendererEvent, ...args: any) => void) => void
      removeDataListener: () => void
      sendToSubprocess: (data: number) => void
    }
  }
}
