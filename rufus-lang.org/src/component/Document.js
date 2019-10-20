import React from 'react';
import ReactMarkdown from 'react-markdown';

class Document extends React.Component {
    constructor() {
        super();
        this.state = {input: ''};
    }

    componentWillMount() {
        this.getData();
    }

    getData() {
        const url = process.env.PUBLIC_URL + '/doc/rdr/0000-rufus-decision-records.md';
        var xhr = new XMLHttpRequest();
        xhr.addEventListener('load', () => {
            this.setState({input: xhr.responseText});
        });
        xhr.open('GET', url);
        xhr.send();
    }

    render() {
        return (
            <ReactMarkdown source={this.state.input} />
        );
    }
}

export default Document;
