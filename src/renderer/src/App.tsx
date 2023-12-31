import { IpcRendererEvent } from 'electron'
import { useEffect, useRef, useState } from 'react'
import defaultParameters from './defaultParameters'
import Split from 'react-split'
import styles from './App.module.css'
import './gutter-styles.css'
import { DataSet } from 'vis-data'
import { graphNames, point } from './graphs'
import ControlColumn from './components/controlColumn'
import DataColumn from './components/dataColumn'

const createGraphDataSet = (rawData: string[]) => {
  let id = 0
  const dataSet = new DataSet<point>()
  const x = rawData[0].split(' ').map(Number)
  for (let i = 1; i < rawData.length; i++) {
    const [y, ...z] = rawData[i].split(' ').map(Number)
    for (let k = 0; k < z.length; k++) {
      dataSet.add({
        id: id++,
        x: x[k],
        y: y,
        z: z[k]
      })
    }
  }
  return dataSet
}

function App() {
  const [dataBeforeGraphs, setDataBeforeGraphs] = useState([] as string[])
  const [graphs, setGraphs] = useState([] as DataSet<point>[])
  const [dataAfterGraphs, setDataAfterGraphs] = useState([] as string[])
  const [waiting, setWaiting] = useState(false)

  //wiping only graphs and data after the graphs
  const cleanData = () => {
    // setDataBeforeGraphs([])
    setDataAfterGraphs([])
    setGraphs([])
  }

  // this object is used to accamulate new unprocessed data and signal generator that to emit it to parsing function
  const incomingData = useRef({
    dataQueue: '', // Queue to store the accumulated raw data
    promise: null as null | Promise<void>, // Promise resolves by event handler to signal that new data is ready
    resolve: null as null | (() => void) // Resolve function for the promise above
  })

  //current generator that yeilds data is stored here, every time fortran program is restarted, a new generator is created to stop the old parsing function and run the new one
  const dataStream = useRef(null as null | ReturnType<typeof dataStreamGenerator>)

  async function* dataStreamGenerator() {
    while (true) {
      const data = incomingData.current.dataQueue
        .split('\n')
        .filter((s) => s !== '')
        .map((line) => line.trim())
      incomingData.current.dataQueue = ''
      for (const s of data) yield s
      if (!incomingData.current.dataQueue) await incomingData.current.promise
    }
  }

  const parseData = async (dataStream: ReturnType<typeof dataStreamGenerator>) => {
    //first the program just gives us parameters we sent, we don't need them obviously
    while ((await dataStream.next()).value !== 'PARAMETERS END') {}

    //first part of data, that prints before the graphs
    while (true) {
      const currentChunk = await dataStream.next()
      if (currentChunk.done) return
      if (currentChunk.value === 'WAIT') break
      setDataBeforeGraphs((data) => [...data, currentChunk.value])
    }

    while (true) {
      setWaiting(true)
      if ((await dataStream.next()).value !== '7') return //check if signal was outputted properly, probably works and the check superfluous
      setWaiting(false)
      cleanData()

      //looks like nothing is printed here, probably does nothing and should be removed
      while (true) {
        const currentChunk = await dataStream.next()
        if (currentChunk.done) return
        if (currentChunk.value === 'BEGIN GRAPH') break
        setDataBeforeGraphs((data) => [...data, currentChunk.value])
      }

      for (const _ of graphNames) {
        const graphData = [] as string[]
        while (true) {
          const currentChunk = await dataStream.next()
          if (currentChunk.done) return
          if (currentChunk.value === '//') break
          graphData.push(currentChunk.value)
        }
        setGraphs((graphs) => [...graphs, createGraphDataSet(graphData)])
      }

      while (true) {
        const currentChunk = await dataStream.next()
        if (currentChunk.done) return
        if (currentChunk.value === 'WAIT') break
        setDataAfterGraphs((data) => [...data, currentChunk.value])
      }
    }
  }

  const handleStartSubprocess = () => {
    // wipe ALL data
    setDataBeforeGraphs([])
    cleanData()

    dataStream.current?.return()
    dataStream.current = dataStreamGenerator()
    parseData(dataStream.current)
    window.api.startPrognozSubprocess(parameters.map(Number))
  }

  useEffect(() => {
    incomingData.current.promise = new Promise((r) => (incomingData.current.resolve = r)) //setting up the first that resolves on the first onData event
    window.api.onData((_: IpcRendererEvent, rawNewData: string) => {
      incomingData.current.dataQueue += rawNewData
      if (incomingData.current.resolve) incomingData.current.resolve() // Resolve the previous promise to indicate new data is available
      incomingData.current.promise = new Promise((r) => (incomingData.current.resolve = r)) // Create a new promise for the next data
    })
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
      <ControlColumn
        parameters={parameters}
        setParameters={setParameters}
        areParametersGood={areParametersGood}
        setAreParametersGood={setAreParametersGood}
        handleStartSubprocess={handleStartSubprocess}
        waiting={waiting}
      />
      <DataColumn
        dataBeforeGraphs={dataBeforeGraphs}
        dataAfterGraphs={dataAfterGraphs}
        graphs={graphs}
      />
    </Split>
  )
}
export default App
