import styles from './Message.module.scss';
import * as React from 'react';

interface MessageProps {
  children: React.ReactNode;
}

const Message: React.FC<MessageProps> = ({ children }) => {
  return (
    <div className={styles.message}>
      <div className={styles.left}>
        <figure className={styles.triangle} />
      </div>
      <div className={styles.right}>
        <div className={styles.bubble}>{children}</div>
      </div>
    </div>
  );
};

export default Message;
