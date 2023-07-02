import { point } from '@renderer/graphs'
import { useEffect, useRef } from 'react'
import { Graph3d } from 'vis-graph3d'
import { DataSet } from 'vis-data'

const Graph = ({ data }: { data: DataSet<point> }) => {
  const graphDiv = useRef(null)
  const options = {
    width: '600px',
    height: '600px',
    style: 'surface',
    showPerspective: true,
    showGrid: true,
    showShadow: false,
    keepAspectRatio: true,
    verticalRatio: 0.5
  }

  useEffect(() => {
    new Graph3d(graphDiv.current, data, options)
  })

  return <div ref={graphDiv} />
}

export default Graph
