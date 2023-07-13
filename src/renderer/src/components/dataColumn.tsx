import { graphNames, point } from '@renderer/graphs'
import Graph from './graph'
import { DataSet } from 'vis-data'

function DataColumn({
  dataBeforeGraphs,
  graphs,
  dataAfterGraphs
}: {
  dataBeforeGraphs: string[]
  graphs: DataSet<point>[]
  dataAfterGraphs: string[]
}) {
  return (
    <div>
      <ul>
        {dataBeforeGraphs.map((line, i) => (
          <li key={i}>{line}</li>
        ))}
      </ul>
      <ul>
        {graphs?.map((data, i) => (
          <li key={i}>
            {graphNames[i]}:
            <Graph data={data} />
          </li>
        ))}
      </ul>
      <ul>
        {dataAfterGraphs.map((line, i) => (
          <li key={i}>{line}</li>
        ))}
      </ul>
    </div>
  )
}

export default DataColumn
