import styles from './Container.module.scss'
import * as React from 'react'

interface ContainerProps extends React.HTMLAttributes<HTMLDivElement> {
  children?: React.ReactNode
}

const Container: React.FC<ContainerProps> = ({ children, ...rest }) => {
  return (
    <div className={styles.container} {...rest}>
      {children}
    </div>
  )
}

export default Container
