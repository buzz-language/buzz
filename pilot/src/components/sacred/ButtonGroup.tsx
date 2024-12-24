'use client';

import styles from './ButtonGroup.module.scss';

import * as React from 'react';
import * as Utilities from '../../common/utilities';

import ActionButton from './ActionButton';

interface ButtonGroupItem {
  id: string;  // Added unique id for key
  hotkey: string;
  onClick?: () => void;
  selected?: boolean;
  body: React.ReactNode;
}

interface ButtonGroupProps {
  items: ButtonGroupItem[];
  isFull?: boolean;
}

const ButtonGroup: React.FC<ButtonGroupProps> = ({ items, isFull }) => {
  if (!items) {
    return null;
  }

  return (
    <div className={Utilities.classNames(styles.root, isFull ? styles.full : null)}>
      {items.map((each) => (
        <ActionButton key={each.id} onClick={each.onClick} hotkey={each.hotkey} isSelected={each.selected}>
          {each.body}
        </ActionButton>
      ))}
    </div>
  );
};

export default ButtonGroup;
