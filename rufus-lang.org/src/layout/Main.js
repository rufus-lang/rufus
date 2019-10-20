import React from 'react';

import MarkdownDocument from './../component/MarkdownDocument';

import './Main.css';

class Main extends React.Component {
    render() {
        return (
            <main className="Main">
                <div className="Main-content">
                    <MarkdownDocument />
                </div>
            </main>
        );
    }
}

export default Main;
