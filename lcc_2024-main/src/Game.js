import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [mode, setMode] = useState('paint');
  const [gameCompleted, setGameCompleted] = useState(false);
  const [rowsSatisfied, setRowsCluesSat] = useState([]);
  const [colsSatisfied, setColsCluesSat] = useState([]);
  const [solutionGrid, setSolutionGrid] = useState([]);
  const [showSolution, setShowSolution] = useState(false);
  const [autoFillMode, setAutoFillMode] = useState(false);

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  useEffect(() => {

    const allRowsSatisfied = rowsSatisfied.every(satisfied => satisfied);
    const allColsSatisfied = colsSatisfied.every(satisfied => satisfied);

    if (allRowsSatisfied && allColsSatisfied) {
      setGameCompleted(true);
    } else {
      setGameCompleted(false);
    }
  }, [rowsSatisfied, colsSatisfied]);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        const rowsClues = response['RowClues'];
        const colsClues = response['ColumClues'];
        const grid = response['Grid'];

        // Verificación de inicialización
        console.log("Rows Clues:", rowsClues);
        console.log("Cols Clues:", colsClues);
        console.log("Grid:", grid);

        if (Array.isArray(rowsClues) && Array.isArray(colsClues) && Array.isArray(grid)) {
          setGrid(grid);
          setRowsClues(rowsClues);
          setColsClues(colsClues);
          setRowsCluesSat(Array(rowsClues.length).fill(false));
          setColsCluesSat(Array(colsClues.length).fill(false));

          
          solveNonogram(rowsClues, colsClues,grid);
          checkInitialClues(rowsClues, colsClues,grid);
        } else {
          console.error("Se recibieron datos no validos de la consulta a Prolog.");
        }
      } else {
        console.error("La consulta init no tuvo éxito.");
      }
      setWaiting(false);
    });
  }

  function checkInitialClues(rowsClues, colsClues,grid){
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const gridS = JSON.stringify(grid);
    const queryS =`check_initial_clues(${rowsCluesS}, ${colsCluesS},${gridS}, StateRowsClues,StateColsClues)`;

    pengine.query(queryS, (success, response) => {
      if (success) {
        setRowsCluesSat(response['StateRowsClues']);
        setColsCluesSat(response['StateColsClues']);

      } else {
        console.error("La consulta check_initial_clues no tuvo éxito.");
      }
    });

  }

  function solveNonogram(rowsClues, colsClues,grid) {
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const gridS = JSON.stringify(grid);
    const queryS = `solveNonogram(${rowsCluesS}, ${colsCluesS},${gridS}, SolutionGrid)`;

    console.log("Calling solveNonogram with:");
    console.log("Rows Clues:", rowsCluesS);
    console.log("Cols Clues:", colsCluesS);
    console.log("Grid:", gridS);

    pengine.query(queryS, (success, response) => {
      if (success) {
        setSolutionGrid(response['SolutionGrid']);
        console.log('Nonogram solved:', response['SolutionGrid']);
      } else {
        console.error("La consulta solveNonogram no tuvo éxito.");
      }
    });
  }

  function handleClick(i, j) {
    if (waiting || gameCompleted || showSolution) {
      return;
    }
    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); 
    let content;
    if (autoFillMode) {
      content = solutionGrid[i][j];
    } else {
      content = mode === 'paint' ? '#' : 'X';
    }

    console.log("SOlucion  " + solutionGrid);
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    console.log(rowsCluesS);

    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`;
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);

        const RowSat = response['RowSat'];
        const ColSat = response['ColSat'];

        const newrowsSatisfied = [...rowsSatisfied];
        newrowsSatisfied[i] = RowSat;
        setRowsCluesSat(newrowsSatisfied);

        const newcolsSatisfied = [...colsSatisfied];
        newcolsSatisfied[j] = ColSat;
        setColsCluesSat(newcolsSatisfied);
      }
      setWaiting(false);
    });
  }

  function handleModeChange() {

    setMode(mode === 'paint' ? 'erase' : 'paint');
  }

  function toggleSolution() {
    setShowSolution(!showSolution);
  }

  function toggleAutoFillMode() {
    setAutoFillMode(!autoFillMode);
  }

  if (!grid) {
    return null;
  }

  let statusText;
  if (rowsSatisfied.every(satisfied => satisfied) && colsSatisfied.every(satisfied => satisfied)) {
    statusText = " ¡Level completed!";
  } else {
    statusText = " Keep playing";
  }

  return (
    <div className="game" style={{ display: 'flex', flexDirection: 'column', justifyContent: 'center', alignItems: 'center', height: '100%', width: '100%' }}>
      <div className="league-spartan-text">
         Nonogram
      </div>
      <Board
        grid={showSolution ? solutionGrid : grid}
        rowsClues={rowsClues}
        colsClues={colsClues}
        onClick={(i, j) => handleClick(i, j)}
        rowsSatisfied={rowsSatisfied}
        colsSatisfied={colsSatisfied}
        gameComplet ed={gameCompleted}
      />
      <div className="game-info" style={{ color: '#ece5f0' }}>
        {statusText}
        <div className="container">
          <input type="checkbox" id="check" className="switch-input" checked={mode === 'paint'} onChange={handleModeChange} />
          <label htmlFor="check" className="button switch-label"></label>
        </div>
        <button onClick={toggleSolution} class="solution">
          {showSolution ? 'Hide Solution' : 'Show Solution'}
        </button>
        <div className="container">
          <input type="checkbox" id="autoFillCheck" className="switch-input" checked={autoFillMode} onChange={toggleAutoFillMode} />
          <label htmlFor="autoFillCheck" className="button switch-label"></label>
        </div>
        <span> auto-fill mode</span>
      </div>
    </div>
  );
}

export default Game;
