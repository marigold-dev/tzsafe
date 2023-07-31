# TzSafe
TzSafe is a multisig wallet aiming at providing better assurance of security and management of ownership than a traditional single-signed wallet. TzSafe adheres to the [TZIP26](https://gitlab.com/tezos/tzip/-/blob/master/drafts/current/draft-proof-of-event/proof_of_event.md) standard, which is known as "Proof of event."

Multi-signature (also multisig) wallet allows to share the ownership of an account by a smart contract. Each owner can create a proposal to propose transferring Tez or executing other contracts. Signer provides approval stored on-chain by performing Tezos transaction. Once gathering the minimal approvals, the multisig will perform the proposal.

# Requirements
The contract is written in cameligo. Please follow [the instructions of installation in LIGO](https://ligolang.org/docs/intro/introduction?lang=cameligo).

The minimal required version can be found by performing `make ligo-version`.

# Usage
## Entrypoints of multisig
### default
This entrypoint can receive Tez from any source.
- Emit event
  - tag: `default`
  - data: `(sender, address)`

### create_proposal
Each owner can create proposal through this entrypoint. The entrypoint supports creating a batch of transactions. The batch is atomic and execution by order. If modifing settings are proposed, the modified setting will NOT apply in this batch immediately. The setting will effect on a next batch/transaction.

- proposal that owner can create
  - `Transfer of { target:address; parameter:unit; amount:tez }`
     - transfer amount only
  - `Execute of { target:address; parameter:'a; amount:tez }`
     - execute contract with type of parameter `'a`
  - `Execute_lambda of { metadata: bytes option; lambda: (unit -> operation)}`
     - execute lambda, note that the cost of using `Transfer` and `Execute`is cheaper than `Execute_lambda`
  - `Adjust_threshold of nat`
     - adjust threshold. the threshold should be >0. Otherwises, errors will occur.
  - `Add_owners of address set`
     - add owners
  - `Remove_owners of address set`
     - remove owners

- Emit event
  - tag: `create_proposal`
  - data: `(proposal id, created proposal)`

### sign_proposal
Signers can provide an approval or a disapproval through this entrypoint.

- Emit event
  - tag: `sign_proposal`
  - data: `(proposal id, owner, agreement)`

### proof_of_event_challenge
The adaptation to the TZIP-26 standard includes renaming the `resolve_proposal` entrypoint as `proof_of_event_challenge`. In this context, the `challenge_id` now represents the `proposal_id`, while the payload corresponds to the content of the proposal.

Signers can resolve proposal when minimal requestment is statisfied.

- Emit event
  - tag: `resolve_proposal`
  - data: `(proposal id, owner)`
  - tag: `proof_of_event`
  - data: `(chellenge_id, payload)`

# Deploy
We provide several steps for quick deploying contracts in `app/` to ghostnet.

1. build contracts by performing `make build`
1. run `make get-tezos-binary` to get tezos-client binary
1. run `make gen-wallet` to generate an implicit account. The account information will show up.
1. go [faucet](https://faucet.marigold.dev/) to request some XTZ.
1. run `make deploy` to deploy contracts

# Changelog
- 0.1.x: (release-1.0)
- 0.2.x: base on 0.1.x and support the ticket (main branch)
- 0.3.x: base on 0.1.x and support TZIP-26 (release-3.0)
