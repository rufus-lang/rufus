import React from 'react';

import Footer from './layout/Footer';
import Header from './layout/Header';
import Main from './layout/Main';

import './App.css';

class App extends React.Component {
    render() {
        return (
            <div className="App">
                <Header />
                <Main />
                <Footer />
            </div>
        );
    }
}

export default App;
