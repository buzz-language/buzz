'use client';

import styles from '@components/PopoverTrigger.module.scss';

import * as React from 'react';

import Popover from '@components/Popover';
import OutsideElementEvent from '@components/detectors/OutsideElementEvent';

import { createPortal } from 'react-dom';

type Placement = 'top' | 'bottom' | 'left' | 'right';

interface PopoverTriggerProps {
  children: React.ReactElement<React.HTMLAttributes<HTMLElement>>;
  text: string;
}

function PopoverTrigger({ children, text }: PopoverTriggerProps) {
  const [open, setOpen] = React.useState(false);
  const [placement, setPlacement] = React.useState<Placement>('bottom');
  const [position, setPosition] = React.useState<{ top: number; left: number }>({ top: 0, left: 0 });

  const triggerRef = React.useRef<HTMLDivElement>(null);
  const popoverRef = React.useRef<HTMLDivElement>(null);

  const onMouseEnter = () => setOpen(true);
  const onClick = () => setOpen(true);
  const onHandleFocus = () => setOpen(true);
  const onOutsideEvent = () => setOpen(false);

  React.useEffect(() => {
    if (!open || !triggerRef.current || !popoverRef.current) return;

    const calculatePosition = () => {
      const triggerRect = triggerRef.current!.getBoundingClientRect();
      const popoverRect = popoverRef.current!.getBoundingClientRect();
      const scrollY = window.scrollY;
      const scrollX = window.scrollX;

      const spaceAbove = triggerRect.top;
      const spaceBelow = window.innerHeight - triggerRect.bottom;
      const spaceLeft = triggerRect.left;
      const spaceRight = window.innerWidth - triggerRect.right;

      const viewportHeightThreshold = window.innerHeight * 0.4;
      const viewportWidthThreshold = window.innerWidth * 0.4;

      let newPlacement: Placement = 'bottom';
      let top = 0;
      let left = 0;

      if (spaceAbove >= viewportHeightThreshold && spaceAbove >= popoverRect.height) {
        newPlacement = 'top';
        top = triggerRect.top + scrollY - popoverRect.height;
        left = triggerRect.left + scrollX + (triggerRect.width - popoverRect.width) / 2;
      } else if (spaceBelow >= viewportHeightThreshold && spaceBelow >= popoverRect.height) {
        newPlacement = 'bottom';
        top = triggerRect.bottom + scrollY;
        left = triggerRect.left + scrollX + (triggerRect.width - popoverRect.width) / 2;
      } else if (spaceRight >= viewportWidthThreshold && spaceRight >= popoverRect.width) {
        newPlacement = 'right';
        top = triggerRect.top + scrollY + (triggerRect.height - popoverRect.height) / 2;
        left = triggerRect.right + scrollX;
      } else if (spaceLeft >= viewportWidthThreshold && spaceLeft >= popoverRect.width) {
        newPlacement = 'left';
        top = triggerRect.top + scrollY + (triggerRect.height - popoverRect.height) / 2;
        left = triggerRect.left + scrollX - popoverRect.width;
      } else {
        newPlacement = 'bottom';
        top = triggerRect.bottom + scrollY;
        left = triggerRect.left + scrollX + (triggerRect.width - popoverRect.width) / 2;
      }

      if (left < 0) left = 0;
      else if (left + popoverRect.width > window.innerWidth) left = window.innerWidth - popoverRect.width;

      if (top < 0) top = 0;
      else if (top + popoverRect.height > window.innerHeight + scrollY) top = window.innerHeight + scrollY - popoverRect.height;

      setPlacement(newPlacement);
      setPosition({ top, left });
    };

    calculatePosition();

    const handleResizeOrScroll = () => calculatePosition();

    window.addEventListener('resize', handleResizeOrScroll);
    window.addEventListener('scroll', handleResizeOrScroll, true);

    return () => {
      window.removeEventListener('resize', handleResizeOrScroll);
      window.removeEventListener('scroll', handleResizeOrScroll, true);
    };
  }, [open]);

  const popoverElement = open
    ? createPortal(
        <OutsideElementEvent onOutsideEvent={onOutsideEvent}>
          <Popover
            ref={popoverRef}
            style={{
              position: 'absolute',
              top: `${position.top}px`,
              left: `${position.left}px`,
              zIndex: `var(--z-index-page-popover)`,
            }}
            role="dialog"
            aria-modal="true"
          >
            {text}
          </Popover>
        </OutsideElementEvent>,
        document.body
      )
    : null;

  return (
    <div ref={triggerRef} className={styles.root} data-detector-ignore onMouseEnter={onMouseEnter} onClick={onClick} onFocus={onHandleFocus}>
      {React.cloneElement(children, {
        tabIndex: 0,
      })}
      {popoverElement}
    </div>
  );
}

export default PopoverTrigger;
