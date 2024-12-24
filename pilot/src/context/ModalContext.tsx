import React, { createContext, useContext, useState } from 'react';

interface ModalContextType {
  modalStack: any[];
  pushModal: (modal: any) => void;
  popModal: () => void;
  close: () => void;  // Added close method
}

const ModalContext = createContext<ModalContextType>({
  modalStack: [],
  pushModal: () => {},
  popModal: () => {},
  close: () => {},  // Added close implementation
});

export const useModals = () => useContext(ModalContext);

export const ModalProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [modalStack, setModalStack] = useState<any[]>([]);

  const pushModal = (modal: any) => {
    setModalStack((prev) => [...prev, modal]);
  };

  const popModal = () => {
    setModalStack((prev) => prev.slice(0, -1));
  };

  return (
    <ModalContext.Provider value={{ modalStack, pushModal, popModal, close: popModal }}>
      {children}
    </ModalContext.Provider>
  );
};
