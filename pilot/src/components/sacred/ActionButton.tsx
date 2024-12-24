import styles from '@components/ActionButton.module.scss';

import * as React from 'react';
import * as Utilities from '@common/utilities';

interface ActionButtonProps {
  onClick?: () => void;
  hotkey?: any;
  children?: React.ReactNode;
  style?: any;
  isSelected?: boolean;
}

const ActionButton: React.FC<ActionButtonProps> = ({ onClick, hotkey, children, style, isSelected }) => {
  return (
    <div className={Utilities.classNames(styles.root, isSelected ? styles.selected : null)} onClick={onClick} tabIndex={0} role="button">
      {Utilities.isEmpty(hotkey) ? null : <span className={styles.hotkey}>{hotkey}</span>}
      <span className={styles.content} style={style}>
        {children}
      </span>
    </div>
  );
};

export default ActionButton;
