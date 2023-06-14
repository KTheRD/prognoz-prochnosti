import { Button } from '@mui/material'
import { IpcRendererEvent } from 'electron'
import { useEffect, useState } from 'react'
import ParametersForm from './components/form'
import defaultParameters from './defaultParameters'
import Split from 'react-split'
import styles from './App.module.css'
import './gutter-styles.css'

const emptyData = {
  didParametersEnd: false,
  isPrintingGraphs: false,
  graphs: '',
  otherOutput: '',
  isWaiting: false
}

function App(): JSX.Element {
  const [data, setData] = useState(emptyData)

  const handleStartSubprocess = () => {
    setData(emptyData)
    window.api.startPrognozSubprocess(parameters.map(Number))
  }

  useEffect(() => {
    window.api.onData((_: IpcRendererEvent, rawNewData: string) =>
      setData((oldData) => {
        const rawDataArr = rawNewData
          .split('\n')
          .filter(Boolean)
          .map((line) => line.trim())
        const newData = { ...oldData }

        for (let line of rawDataArr) {
          if (!newData.didParametersEnd) {
            if (line === 'PARAMETERS END') {
              newData.didParametersEnd = true
            }
            continue
          }
          if (newData.isWaiting) {
            if (line === 'OK') {
              newData.isWaiting = false
            }
            continue
          }
          if (line === 'WAIT') {
            newData.isWaiting = true
            continue
          }
          if (line === 'BEGIN PECH') {
            newData.isPrintingGraphs = true
            newData.graphs = ''
            continue
          }
          if (newData.isPrintingGraphs) {
            if (line === 'END PECH') {
              newData.isPrintingGraphs = false
              continue
            }
            newData.graphs += `${line}\n`
            continue
          }
          newData.otherOutput += `${line}\n`
        }

        return newData
      })
    )
    return window.api.removeDataListener
  }, [])

  const [parameters, setParameters] = useState(
    defaultParameters.map((p) => p.defaultValue.toString()) as string[]
  )

  const [areParametersGood, setAreParametersGood] = useState(
    defaultParameters.map(() => true) as boolean[]
  )

  return (
    <Split className={styles.split} gutterSize={5}>
      <div className={styles.column}>
        <ParametersForm
          {...{
            parameters,
            setParameters,
            good: areParametersGood,
            setGood: setAreParametersGood
          }}
        />
        <div>
          <Button onClick={handleStartSubprocess}>Start</Button>
          {data.didParametersEnd && (
            <Button onClick={() => window.api.sendToSubprocess(7)}>000</Button>
          )}
        </div>
      </div>
      <div className={styles.column}>
        <ul>
          {data.otherOutput.split('\n').map((line) => (
            <li>{line.split(/\s+/).join(' ')}</li>
          ))}
        </ul>
        <ul>
          {data.graphs.split('\n').map((line) => (
            <li>{line.split(/\s+/).join(' ')}</li>
          ))}
        </ul>
      </div>
    </Split>
  )
}
export default App
