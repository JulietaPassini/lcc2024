import React from 'react';

function Square({ value, onClick }) {
    let className = "square";
    if ( value === '#') {
        className += " paint";
    }

    return (
        <button className={className} onClick={onClick}>
            {value !== '_' ? value : null}
        </button>
    );
}

export default Square;


