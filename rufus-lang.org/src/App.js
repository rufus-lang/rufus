import React from 'react';

import Contents from './Contents';
import Footer from './Footer';
import Navigation from './Navigation';
import './App.css';

function App() {
    return (
        <div className="App">
            <Navigation />
            <Contents />
            <Footer />
        </div>
    );
}

export default App;
