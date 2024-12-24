import styles from './Container.module.scss'
import * as React from 'react'
import * as Utilities from '@common/utilities'

interface ContainerProps extends React.HTMLAttributes<HTMLDivElement> {
  children?: React.ReactNode
  className?: string
  isMain?: boolean
}

const Container: React.FC<ContainerProps> = ({
  children,
  className,
  isMain,
  ...rest
}) => {
  return (
    <div
      className={Utilities.classNames(
        styles.container,
        isMain && styles.mainContainer,
        className
      )}
      {...rest}
    >
      {children}
    </div>
  )
}

export default Container
