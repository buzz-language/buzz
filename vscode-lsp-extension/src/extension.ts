import { workspace, ExtensionContext, window } from 'vscode'

import * as vscode from 'vscode'
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from 'vscode-languageclient/node'

let outputChannel: vscode.OutputChannel
let client: LanguageClient | null = null

export function activate (context: ExtensionContext) {
  outputChannel = window.createOutputChannel('buzz Language Server')

  vscode.commands.registerCommand('buzz.start', async () => {
    await startClient(context)
  })

  vscode.commands.registerCommand('buzz.stop', async () => {
    await stopClient()
  })

  vscode.commands.registerCommand('buzz.restart', async () => {
    await stopClient()
    await startClient(context)
  })

  startClient(context)
}

function startClient (context: ExtensionContext): Promise<void> {
  const configuration = workspace.getConfiguration('buzz')
  const lspPath = configuration.get(
    'path',
    '/Users/giann/git/buzz/tools/lsp.sh'
  )

  let serverOptions: ServerOptions = {
    command: lspPath,
    args: []
  }

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'buzz' }],
    outputChannel
  }

  // Create the language client and start the client.
  client = new LanguageClient(
    'buzz',
    'buzz Language Server',
    serverOptions,
    clientOptions
  )

  outputChannel.appendLine(`Attempting to use buzz with ${lspPath}`)

  return new Promise<void>(resolve => {
    if (client)
      client
        .start()
        .catch(err => {
          window.showErrorMessage(err)
          client = null
        })
        .then(() => {
          if (client) {
            window.showInformationMessage('buzz language client started!')
            resolve()
          }
        })
  })
}

async function stopClient (): Promise<void> {
  if (client) await client.stop()
  window.showInformationMessage('buzz language client stopped!')
}

export function deactivate (): Thenable<void> {
  return stopClient()
}
