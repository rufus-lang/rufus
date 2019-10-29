import React from 'react';
import ReactMarkdown from 'react-markdown';

import './MarkdownDocument.css';

class MarkdownDocument extends React.Component {
    constructor(props) {
        super(props);
        this.state = {input: ''};
    }

    componentDidMount() {
        this.getData();
    }

    getData() {
        const pathname = document.location.pathname;
        const markdownPathname = pathname === "/" ? "/index.md" : pathname + ".md";
        const url = process.env.PUBLIC_URL + markdownPathname;
        var xhr = new XMLHttpRequest();
        xhr.addEventListener('load', () => {
            this.setState({input: xhr.responseText});
        });
        xhr.open('GET', url);
        xhr.onerror = function () {
            console.log("** An error occurred during the HTTP request to load Markdown content **");
            console.log("status: " + xhr.status);
            console.log("statusText: " + xhr.statusText);
        };
        xhr.send();
    }

    render() {
        return (
            <div className="MarkdownDocument">
                <ReactMarkdown source={this.state.input} />
            </div>
        );
    }
}

export default MarkdownDocument;
