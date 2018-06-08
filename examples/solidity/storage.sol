// Taken from https://medium.com/@gus_tavo_guim/deploying-a-smart-contract-the-hard-way-8aae778d4f2a

pragma solidity ^0.4.10;

contract Storage {
  uint256 storedData;

  function set(uint256 data) {
    storedData = data;
  }

  function get() constant returns (uint256) {
    return storedData;
  }
}