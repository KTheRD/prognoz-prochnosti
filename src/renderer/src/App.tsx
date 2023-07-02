import { Button } from '@mui/material'
import { IpcRendererEvent } from 'electron'
import { useEffect, useState } from 'react'
import ParametersForm from './components/form'
import defaultParameters from './defaultParameters'
import Split from 'react-split'
import styles from './App.module.css'
import './gutter-styles.css'
import { DataSet } from 'vis-data'
import { graphNames, point } from './graphs'
import Graph from './components/graph'

const emptyData = {
  didParametersEnd: false,
  isPrintingGraphs: false,
  rawGraphs: [] as string[],
  graphs: null as null | DataSet<point>[],
  otherOutput: '',
  isWaiting: false
}

const parseGraphs = (rawGraphs: string[]) => {
  let i = 0
  let id = 0
  const graphs = [] as DataSet<point>[]

  console.log(rawGraphs)
  for (let j = 0; j < graphNames.length; j++) {
    graphs.push(new DataSet<point>())
    const x = rawGraphs[i].split(' ').map(Number)
    i++
    while (rawGraphs[i] !== '//') {
      const [y, ...z] = rawGraphs[i].split(' ').map(Number)
      z.forEach((z, k) =>
        graphs[graphs.length - 1].add({
          id: id++,
          x: x[k],
          y: y,
          z: z
        })
      )
      i++
    }
    i++
  }
  return graphs
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
              window.api.sendToSubprocess(7)
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
            newData.rawGraphs = []
            continue
          }
          if (newData.isPrintingGraphs) {
            if (line === 'END PECH') {
              newData.isPrintingGraphs = false
              newData.graphs = parseGraphs(newData.rawGraphs)
              continue
            }
            newData.rawGraphs.push(line)
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
          <Button onClick={handleStartSubprocess}>Старт</Button>
          {data.didParametersEnd && (
            <Button onClick={() => window.api.sendToSubprocess(7)}>Продолжить</Button>
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
          {/* {data.rawGraphs.map((line) => ( */}
          {/*   <li>{line.split(/\s+/).join(' ')}</li> */}
          {/* ))} */}
          {data.graphs?.map((data, i) => (
            <li>
              {graphNames[i]}:
              <Graph data={data} />
            </li>
          ))}
        </ul>
      </div>
    </Split>
  )
}
export default App
