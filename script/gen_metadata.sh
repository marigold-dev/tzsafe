#!/bin/bash

echo '
{
    "name": "TzSafe",
    "description": "TzSafe is a multisig wallet aiming at providing better assurance of security and management of ownership than a traditional single-signed wallet",
    "version": "'${VERSION}'",
    "license": {
        "name": "MIT"
    },
    "authors": [
        "Marigold <contract@marigold.dev>"
    ],
    "homepage": "https://www.marigold.dev/tzsafe",
    "source": {
        "tools": "cameligo",
        "location": "https://github.com/marigold-dev/tzsafe/"
    },
    "interfaces": [
        "TZIP-016"
    ]
}'
