import styles from './Divider.module.scss';

import * as React from 'react';

interface DividerProps extends React.HTMLAttributes<HTMLSpanElement> {
  children?: React.ReactNode;
  type?: string | any;
}

const Divider: React.FC<DividerProps> = ({ children, type }) => {
  if (type === 'GRADIENT') {
    return <div className={styles.gradient} />;
  }

  if (type === 'DOUBLE') {
    return (
      <div className={styles.divider}>
        <div className={styles.line} style={{ marginBottom: `2px` }} />
        <div className={styles.line} />
      </div>
    );
  }

  return (
    <div className={styles.divider}>
      <div className={styles.line} />
    </div>
  );
};

export default Divider;
