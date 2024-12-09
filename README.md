# HoneyBadger Framework - Demo

HoneyBadger is a highly-optimized Solidity development framework that makes it easy to build cutting-edge Solidity systems by streamlining development requirements and providing powerful capabilities right out of the box.  With HoneyBadger, developers can leverage powerful utilities including pre-built permission management, batch operations, governance modules, and more as they write efficient pure-logic contracts, leveraging HoneyBadger as their storage layer.  Additionally, HoneyBadger provides a new approach to upgradeability, enabling in-place storage space and mapping extension, providing a significant advantage over traditional proxy-delegate models.

This demo version showcases HoneyBadger's advanced implementation, without revealing any of the tricks up our sleeve.  This model is barebones and lacks most of the features that makes HoneyBadger unique, but does showcase the system's syntax and workflow.


Project frontend: https://www.honeybadgerframework.com


Feel free to contact me: https://www.linkedin.com/in/cameron-warnick-64a25222a/

Linkedin company page: https://www.linkedin.com/company/honeybadgerframework/?viewAsMember=true

Project twitter: https://x.com/HoneyBadgerWeb3
## Deployment

The demo version is easy to use. Simply deploy the contract and use the init function to define a storage space.

Init expects the following:

types: <uint256>[],
sizes: <uint256>[],
types.length === sizes.length

types: 

1 - uint 

2 - int

3 - bool 

4 - address

5 - bytes32 (for special uses, uint256 is functionally identical).

Then, you need to push an entry to the system by invoking 'push'

Now, you can store a value to the system

ie; put(1234, 1, 0) 

1234: value

1: member index (ie; the second value in our mapping)

0: entry index -- ie; user with id 0



## Authors

- [@wisecameron](https://www.github.com/wisecameron)
