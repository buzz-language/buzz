'use client';

import styles from './TooltipTrigger.module.scss';

import * as React from 'react';

import Tooltip from './Tooltip';
import OutsideElementEvent from './detectors/OutsideElementEvent';

import { createPortal } from 'react-dom';

type Placement = 'top' | 'bottom' | 'left' | 'right';

interface TooltipTriggerProps {
  children: React.ReactElement<React.HTMLAttributes<HTMLElement>>;
  text: string;
}

function TooltipTrigger({ children, text }: TooltipTriggerProps) {
  const [open, setOpen] = React.useState(false);
  const [placement, setPlacement] = React.useState<Placement>('bottom');
  const [position, setPosition] = React.useState<{ top: number; left: number }>({ top: 0, left: 0 });

  const triggerRef = React.useRef<HTMLDivElement>(null);
  const tooltipRef = React.useRef<HTMLDivElement>(null);

  const onMouseEnter = () => setOpen(true);
  const onClick = () => setOpen(true);
  const onHandleFocus = () => setOpen(true);
  const onOutsideEvent = () => setOpen(false);

  React.useEffect(() => {
    if (!open || !triggerRef.current || !tooltipRef.current) return;

    const calculatePosition = () => {
      const triggerRect = triggerRef.current!.getBoundingClientRect();
      const tooltipRect = tooltipRef.current!.getBoundingClientRect();
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

      if (spaceAbove >= viewportHeightThreshold && spaceAbove >= tooltipRect.height) {
        newPlacement = 'top';
        top = triggerRect.top + scrollY - tooltipRect.height;
        left = triggerRect.left + scrollX + (triggerRect.width - tooltipRect.width) / 2;
      } else if (spaceBelow >= viewportHeightThreshold && spaceBelow >= tooltipRect.height) {
        newPlacement = 'bottom';
        top = triggerRect.bottom + scrollY;
        left = triggerRect.left + scrollX + (triggerRect.width - tooltipRect.width) / 2;
      } else if (spaceRight >= viewportWidthThreshold && spaceRight >= tooltipRect.width) {
        newPlacement = 'right';
        top = triggerRect.top + scrollY + (triggerRect.height - tooltipRect.height) / 2;
        left = triggerRect.right + scrollX;
      } else if (spaceLeft >= viewportWidthThreshold && spaceLeft >= tooltipRect.width) {
        newPlacement = 'left';
        top = triggerRect.top + scrollY + (triggerRect.height - tooltipRect.height) / 2;
        left = triggerRect.left + scrollX - tooltipRect.width;
      } else {
        newPlacement = 'bottom';
        top = triggerRect.bottom + scrollY;
        left = triggerRect.left + scrollX + (triggerRect.width - tooltipRect.width) / 2;
      }

      if (left < 0) left = 0;
      else if (left + tooltipRect.width > window.innerWidth) left = window.innerWidth - tooltipRect.width;

      if (top < 0) top = 0;
      else if (top + tooltipRect.height > window.innerHeight + scrollY) top = window.innerHeight + scrollY - tooltipRect.height;

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

  const tooltipElement = open
    ? createPortal(
        <OutsideElementEvent onOutsideEvent={onOutsideEvent}>
          <Tooltip
            ref={tooltipRef}
            style={{
              position: 'absolute',
              top: `${position.top}px`,
              left: `${position.left}px`,
              zIndex: `var(--z-index-page-tooltip)`,
            }}
            role="dialog"
            aria-modal="true"
          >
            {text}
          </Tooltip>
        </OutsideElementEvent>,
        document.body
      )
    : null;

  return (
    <div ref={triggerRef} className={styles.root} data-detector-ignore onMouseEnter={onMouseEnter} onClick={onClick} onFocus={onHandleFocus}>
      {React.cloneElement(children, {
        tabIndex: 0,
      })}
      {tooltipElement}
    </div>
  );
}

export default TooltipTrigger;
