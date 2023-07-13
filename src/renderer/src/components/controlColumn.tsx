import { Button } from '@mui/material'
import ParametersForm from './form'

function ControlColumn({
  parameters,
  setParameters,
  areParametersGood,
  setAreParametersGood,
  handleStartSubprocess,
  waiting
}:{
    parameters:string[]
    setParameters:React.Dispatch<React.SetStateAction<string[]>>
    areParametersGood:boolean[]
    setAreParametersGood:React.Dispatch<React.SetStateAction<boolean[]>>
    handleStartSubprocess:()=>void
    waiting:boolean
  }) {
  return (
    <div>
      <ParametersForm
        {...{
          parameters,
          setParameters,
          good: areParametersGood,
          setGood: setAreParametersGood
        }}
      />
      <div>
        <Button
          onClick={handleStartSubprocess}
          disabled={!areParametersGood.reduce((f, n) => f && n, true)}
        >
          Старт
        </Button>
        {waiting && <Button onClick={() => window.api.sendToSubprocess(7)}>Продолжить</Button>}
      </div>
    </div>
  )
}

export default ControlColumn
