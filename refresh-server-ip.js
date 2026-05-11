#!/usr/bin/env node

const { execFileSync, spawnSync } = require('node:child_process');
const os = require('node:os');

const STORAGE_CLI = '/opt/cronicle/bin/storage-cli.js';
const NODE = process.execPath;

function getRuntimeHostname() {
  return (process.env.CRONICLE_hostname || process.env.HOSTNAME || process.env.HOST || os.hostname()).toLowerCase();
}

function getRuntimeIp() {
  const interfaces = os.networkInterfaces();

  for (const infos of Object.values(interfaces)) {
    if (!infos) continue;

    for (const info of infos) {
      if (!info || info.internal) continue;
      if (info.family === 'IPv4' && /^\d+\.\d+\.\d+\.\d+$/.test(info.address)) {
        return info.address;
      }
    }
  }

  throw new Error('Unable to determine runtime IPv4 address');
}

function runStorageGet(key) {
  const output = execFileSync(NODE, [STORAGE_CLI, 'get', key], {
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  });

  return JSON.parse(output);
}

function runStoragePut(key, value) {
  const result = spawnSync(NODE, [STORAGE_CLI, 'put', key], {
    input: `${JSON.stringify(value)}\n`,
    encoding: 'utf8',
    stdio: ['pipe', 'pipe', 'pipe'],
  });

  if (result.status !== 0) {
    const stderr = (result.stderr || '').trim();
    const stdout = (result.stdout || '').trim();
    throw new Error(stderr || stdout || `storage-cli put failed for ${key}`);
  }
}

function main() {
  const hostname = getRuntimeHostname();
  const ip = getRuntimeIp();

  let header;
  try {
    header = runStorageGet('global/servers');
  }
  catch (error) {
    console.warn(`[refresh-server-ip] Skipping server IP refresh: ${error.message}`);
    return;
  }

  const firstPage = Number.isInteger(header.first_page) ? header.first_page : 0;
  const lastPage = Number.isInteger(header.last_page) ? header.last_page : 0;

  for (let page = firstPage; page <= lastPage; page += 1) {
    const key = `global/servers/${page}`;
    const pageData = runStorageGet(key);

    if (!Array.isArray(pageData.items)) continue;

    const server = pageData.items.find((item) => item && item.hostname === hostname);
    if (!server) continue;

    if (server.ip === ip) {
      console.log(`[refresh-server-ip] Server record already current for ${hostname} -> ${ip}`);
      return;
    }

    server.ip = ip;
    runStoragePut(key, pageData);
    console.log(`[refresh-server-ip] Updated server record for ${hostname}: ${key} -> ${ip}`);
    return;
  }

  console.warn(`[refresh-server-ip] No server record found for hostname ${hostname}; leaving storage unchanged`);
}

main();
