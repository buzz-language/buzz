import styles from './Text.module.scss';

import * as React from 'react';
import * as Utilities from '@common/utilities';

interface TextProps extends React.HTMLAttributes<HTMLElement> {
  children?: React.ReactNode;
  variant?: 'h1' | 'h2' | 'h3' | 'p';
  color?: 'default' | 'secondary' | 'tertiary';
}

const Text: React.FC<TextProps> = ({
  children,
  variant = 'p',
  color = 'default',
  className,
  ...rest
}) => {
  const Element = variant;
  const colorClass = color !== 'default' ? styles[color] : '';

  return (
    <Element
      className={Utilities.classNames(
        styles.text,
        styles[variant],
        colorClass,
        className
      )}
      {...rest}
    >
      {children}
    </Element>
  );
};

export default Text;
