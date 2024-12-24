import styles from './MessageViewer.module.scss';
import * as React from 'react';

interface MessageViewerProps {
  children: React.ReactNode;
}

const MessageViewer: React.FC<MessageViewerProps> = ({ children }) => {
  return (
    <div className={styles.message}>
      <div className={styles.left}>
        <div className={styles.bubble}>{children}</div>
      </div>
      <div className={styles.right}>
        <figure className={styles.triangle} />
      </div>
    </div>
  );
};

export default MessageViewer;
