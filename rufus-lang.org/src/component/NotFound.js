import React from 'react';

import MarkdownDocument from './MarkdownDocument';

class NotFound extends React.Component {
    render() {
        return (
            <MarkdownDocument path="/error/not-found.md" />
        );
    }
}

export default NotFound;
