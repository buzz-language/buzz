import styles from './ActionBar.module.scss';

import * as React from 'react';
import * as Utilities from '../../common/utilities';

import ButtonGroup from './ButtonGroup';

interface ActionBarItem {
  id: string;  // Added id field to match ButtonGroup interface
  hotkey: string;
  onClick?: () => void;
  selected?: boolean;
  body: React.ReactNode;
}

interface ActionBarProps {
  items: ActionBarItem[];
  children?: React.ReactNode;
}

const ActionBar: React.FC<ActionBarProps> = ({ items, children }) => {
  return (
    <div className={styles.root}>
      {children}
      <ButtonGroup items={items} />
    </div>
  );
};

export default ActionBar;
