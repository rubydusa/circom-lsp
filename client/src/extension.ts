import * as vscode from 'vscode';
import { sync as commandExists } from 'command-exists';
import { exec } from 'child_process';
import { CratesIO } from 'crates.io';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from 'vscode-languageclient/node';

type SemVer = [number, number, number];
// I miss rust so much
type Result<T, E> = {
	type: 'success',
	value: T
} | {
	type: 'error',
	error: E
};

enum ExtensionErrorType {
	serverNotInstalled = "LSP Server not installed",
	invalidBinaryVersion = "Invalid LSP server binary"
}

type ExtensionError = {
	type: ExtensionErrorType.serverNotInstalled
} | {
	type: ExtensionErrorType.invalidBinaryVersion
};

enum ExtensionWarningType {
	outdatedServerVersion = "LSP server is outdated"
}

type ExtensionWarning = {
	type: ExtensionWarningType.outdatedServerVersion,
	oldVersion: SemVer,
	newVersion: SemVer
};

type ClientResult = {
	client: LanguageClient,
	warnings: ExtensionWarning[];
};

let globalClient: LanguageClient;
const COMMAND = 'circom-lsp';
const COMMAND_NAME = 'Circom LSP';
const NO_VERSION_COMMAND = [0, 1, 2];

export async function activate(context: vscode.ExtensionContext) {
	const client = await createClient();
	if (client.type === 'error') {
		vscode.window.showErrorMessage(errorMessage(client.error));
	}
	else {
		for (const warning of client.value.warnings) {
			vscode.window.showWarningMessage(warningMessage(warning));
		}
		globalClient = client.value.client;
		globalClient.start();
	}
}

export function deactivate(): Thenable<void> | undefined {
	if (!globalClient) {
		return undefined;
	}
	return globalClient.stop();
}


async function createClient(): Promise<Result<ClientResult, ExtensionError>> {
	const warnings: ExtensionWarning[] = new Array();

	if (!commandExists(COMMAND)) {
		return { 
			type: 'error',
			error: {
				type: ExtensionErrorType.serverNotInstalled
			}
		};
	}

	let currentVersion: Result<number[], ExtensionError> | null = await execPromise(`${COMMAND} --version`)
		.then(version => {
			const match = version.match(/[0-9]+\.[0-9]+\.[0-9]+/);
			if (match === null) {
				return {
					type: 'error',
					error: {
						type: ExtensionErrorType.invalidBinaryVersion
					}
				} as Result<number[], ExtensionError>;
			}

			const splitted = match[0].split('.').map(x => parseInt(x));
			if (splitted.length !== 3) {
				return {
					type: 'error',
					error: {
						type: ExtensionErrorType.invalidBinaryVersion
					}
				} as Result<number[], ExtensionError>;
			}

			return {
				type: 'success',
				value: splitted
			} as Result<number[], ExtensionError>;
		})
		.catch(() => null);
	
	if (currentVersion?.type === 'error') {
		return {
			type: 'error',
			error: currentVersion.error
		};
	}

	// prior to version 0.1.3 there wasn't a version command.
	// tries getting through cargo the version, if cannot be found returns null
	currentVersion = currentVersion !== null
		? currentVersion
		: await execPromise('cargo install --list')
			.then(binaryList => {
				const match = binaryList.match(/circom-lsp\sv([0-9]+\.[0-9]+\.[0-9]+)/);
				if (match === null) {
					return {
						type: 'success',
						value: NO_VERSION_COMMAND
					} as Result<number[], ExtensionError>;
				}

				const splitted = match[1].split('.').map(x => parseInt(x));
				if (splitted.length !== 3) {
					return {
						type: 'error',
						error: {
							type: ExtensionErrorType.invalidBinaryVersion
						}
					} as Result<number[], ExtensionError>;
				}

				return {
					type: 'success',
					value: splitted
				} as Result<number[], ExtensionError>;
			})
			.catch(() => null);
	
	if (currentVersion?.type === 'error') {
		return {
			type: 'error',
			error: currentVersion.error
		};
	}
	else if (currentVersion !== null) {
		const cratesIO = new CratesIO();
		const crateData = await cratesIO.api.crates.getCrate(COMMAND);
		const [newMAJOR, newMINOR, newPATCH] = crateData.crate.max_version.split('.').map(x => parseInt(x));
		const [oldMAJOR, oldMINOR, oldPATCH] = currentVersion.value;

		if (isOlderSemver([oldMAJOR, oldMINOR, oldPATCH], [newMAJOR, newMINOR, newPATCH])) {
			warnings.push({
				type: ExtensionWarningType.outdatedServerVersion,
				oldVersion: [oldMAJOR, oldMINOR, oldPATCH],
				newVersion: [newMAJOR, newMINOR, newPATCH]
			});
		}
	}

	const serverOptions: ServerOptions = {
		command: COMMAND
	};
	
	const clientOptions: LanguageClientOptions = {
		documentSelector: [
			{
				scheme: 'file',
				language: 'circom'
			}
		]
	};

	const client = new LanguageClient(COMMAND, COMMAND_NAME, serverOptions, clientOptions);
	return {
		type: 'success',
		value : {
			client,
			warnings
		}
	};
}

function isOlderSemver([oldMAJOR, oldMINOR, oldPATCH]: SemVer, [newMAJOR, newMINOR, newPATCH]: SemVer): boolean {
	const result = 
		(oldMAJOR < newMAJOR) ||
		(oldMAJOR === newMAJOR && oldMINOR < newMINOR) ||
		(oldMAJOR === newMAJOR && oldMINOR === newMINOR && oldPATCH < newPATCH);
	
	return result;
}

function errorMessage(err: ExtensionError): string {
	throw err.type;
}

function warningMessage(warning: ExtensionWarning): string {
	if (warning.type === ExtensionWarningType.outdatedServerVersion) {
		const current = `${warning.oldVersion[0]}.${warning.oldVersion[1]}.${warning.oldVersion[2]}`;
		const latest = `${warning.newVersion[0]}.${warning.newVersion[1]}.${warning.newVersion[2]}`;
		return `${warning.type}. Current: ${current}, Latest: ${latest}`;
	}

	return warning.type;
}

function execPromise(cmd: string): Promise<string> {
	return new Promise((resolve, reject) => {
		exec(cmd, (error, stdout, stderr) => {
			if (error || stderr) {
				reject(error || stderr);
			}
			resolve(stdout);
		});
	});
}