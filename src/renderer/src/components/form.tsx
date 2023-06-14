import defaultParameters from '@renderer/defaultParameters'
import ParameterInput from './parameterInput'
import styles from './form.module.css'

function ParametersForm({
  parameters,
  setParameters,
  good,
  setGood
}: {
  parameters: string[]
  setParameters: React.Dispatch<React.SetStateAction<typeof parameters>>
  good: boolean[]
  setGood: React.Dispatch<React.SetStateAction<typeof good>>
}) {
  function verificationHandle(good: boolean, id: number) {
    setGood((oldGood) => {
      const newGood = [...oldGood]
      newGood[id] = good
      return newGood
    })
  }

  function changeValueHandle(value: string, id: number) {
    setParameters((oldData) => {
      const newData = [...oldData]
      newData[id] = value
      return newData
    })
  }

  return (
    <div className={styles.container}>
      {defaultParameters.map((p, i) => (
        <ParameterInput
          good={good[i]}
          verificationHandle={verificationHandle}
          name={p.name}
          value={parameters[i]}
          changeValueHandle={changeValueHandle}
          isInteger={p.isInteger}
          defaultValue={p.defaultValue.toString()}
          id={i}
        />
      ))}
    </div>
  )
}

export default ParametersForm
