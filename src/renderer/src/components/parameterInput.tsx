import { TextField } from '@mui/material'
import { useRef } from 'react'
import styles from './form.module.css'

function ParameterInput({
  good,
  verificationHandle,
  name,
  value,
  changeValueHandle,
  isInteger,
  defaultValue,
  id
}: {
  good: boolean
  verificationHandle: (good: boolean, id: number) => void
  name: string
  value: string
  changeValueHandle: (value: string, id: number) => void
  isInteger: boolean
  defaultValue: string
  id: number
}) {
  function changeHandle(e: React.ChangeEvent<HTMLTextAreaElement | HTMLInputElement>) {
    if (isInteger) {
      if (!/^\d*$/.test(e.target.value)) return //test for integer
    } else {
      if (!/^\d*\.?\d*[eE]?[+-]?\d*$/.test(e.target.value)) return //test for a part of any non-negative number inlcuding scientific notation
    }
    if (
      isNaN(Number(e.target.value)) ||
      !isFinite(Number(e.target.value)) ||
      Number(e.target.value) < 0 ||
      e.target.value === ''
    ) {
      if (good) {
        verificationHandle(false, id)
      }
    } else {
      if (!good) {
        verificationHandle(true, id)
      }
    }
    changeValueHandle(e.target.value, id)
  }

  function keyHandle(e: React.KeyboardEvent<HTMLDivElement>) {
    if (e.key === 'Escape') {
      ref.current?.blur()
      changeValueHandle(defaultValue, id)
      verificationHandle(true, id)
    }
  }

  const ref = useRef<HTMLInputElement>(null)

  return (
    <TextField
      className={styles.input}
      inputRef={ref}
      inputProps={{ spellCheck: 'false' }}
      error={!good}
      label={name}
      value={value}
      onChange={changeHandle}
      onKeyDown={keyHandle}
    />
  )
}

export default ParameterInput
