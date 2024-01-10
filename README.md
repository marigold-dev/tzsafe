# TzSafe
TzSafe is a multisig wallet aiming at providing better assurance of security and management of ownership than a traditional single-signed wallet. TzSafe adheres to the [TZIP27](https://gitlab.com/tezos/tzip/-/blob/master/drafts/current/draft-proof-of-event/proof_of_event.md) standard, which is the way of signing message for account abstraction.

Multi-signature (also multisig) wallet allows to share the ownership of an account by a smart contract. Each owner can create a proposal to propose transferring Tez or executing other contracts. Signer provides approval stored on-chain by performing Tezos transaction. Once gathering the minimal approvals, the multisig will perform the proposal.

# Requirements
The contract is written in cameligo. Please follow [the instructions of installation in LIGO](https://ligolang.org/docs/intro/introduction?lang=cameligo).

The minimal required version can be found by performing `make ligo-version`.

# Usage
## Entrypoints of multisig
### default
This entrypoint can receive Tez from any source.

### create_proposal
Each owner can create proposal through this entrypoint. The entrypoint supports creating a batch of transactions. The batch is atomic and execution by order. If modifing settings are proposed, the modified setting will NOT apply in this batch immediately. The setting will effect on a next batch/transaction. Once a proposal is created, its ID can be located in the corresponding emitted event and can be used for both the signing and resolution of the created proposal.

### sign_proposal
Signers can provide an approval or a disapproval through this entrypoint.

### resolve_proposal
Through this entrypoint, owners have the capability to resolve a proposal. Whether the proposal is executed, rejected, or expires, the allocated storage space will consequently be freed.

### proof_of_event_challenge
If TzSafe requires signing message, the contract owner can process the proof-of-event challenge by initiating a proof-of-event proposal through this entrypoint. This proposal, once created, necessitates signing and resolution as the other regular proposals. Following the resolution of the proposal, an event with tag "%proof_of_event" will be emitted, serving as the equivalent of the signature.

# Deploy
We provide several steps for quick deploying contracts in `app/` to ghostnet.

1. build contracts by performing `make build`
1. run `make get-tezos-binary` to get tezos-client binary
1. run `make gen-wallet` to generate an implicit account. The account information will show up.
1. go [faucet](https://faucet.marigold.dev/) to request some XTZ.
1. run `make deploy` to deploy contracts

# Changelog
- 0.1.x: (release-1.0)
- 0.2.x: base on 0.1.x and support the ticket (release-2.0)
- 0.3.x: base on 0.1.x, better storage space management and support TZIP-27 (release-3.0)
