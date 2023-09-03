/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import {
    workspace,
    window,
} from "vscode";

import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export async function activate() {
    const traceOutputChannel = window.createOutputChannel("NTLC Language Server trace");
    const command = process.env.SERVER_PATH || "ntlc_lsp";
    const run: Executable = {
        command,
        options: {
            env: {
                ...process.env,
                RUST_LOG: "debug",
            },
        },
    };
    const serverOptions: ServerOptions = {
        run,
        debug: run,
    };
    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: "file", language: "ntlc" }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher("**/.ntlc"),
        },
        traceOutputChannel,
    };

    traceOutputChannel.appendLine(`Starting NTLC Language Server: [${process.env.SERVER_PATH}]`);
    // Create the language client and start the client.
    client = new LanguageClient("ntlc_lsp", "NTLC language server", serverOptions, clientOptions);
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
