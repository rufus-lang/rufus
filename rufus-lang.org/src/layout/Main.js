import React from 'react';

import Document from './../component/Document';

import './Main.css';

class Main extends React.Component {
    render() {
        return (
            <main className="Main">
                <div className="Main-content">
                    <Document />
                </div>
            </main>
        );
    }
}

export default Main;
