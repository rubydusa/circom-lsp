"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.deactivate = exports.activate = void 0;
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    let serverOptions = {
        command: 'circom-lsp'
    };
    const clientOptions = {
        documentSelector: [
            {
                scheme: 'file',
                language: 'circom'
            }
        ]
    };
    client = new node_1.LanguageClient('circom-lsp', 'Circom LSP', serverOptions, clientOptions);
    client.start();
}
exports.activate = activate;
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
exports.deactivate = deactivate;
//# sourceMappingURL=extension.js.map