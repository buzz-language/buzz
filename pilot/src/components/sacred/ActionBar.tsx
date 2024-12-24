import styles from './ActionBar.module.scss'
import * as React from 'react'

interface ActionBarProps {
  children?: React.ReactNode
}

const ActionBar: React.FC<ActionBarProps> = ({ children }) => {
  return (
    <div className={styles.root}>
      {children}
    </div>
  )
}

export default ActionBar
