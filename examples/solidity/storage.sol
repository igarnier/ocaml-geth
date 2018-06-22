pragma solidity ^0.4.10;

contract Storage {

  event Set(uint256);

  uint256 storedData;
  string  message;

  function Storage(uint256 initialData, string initialMessage) {
    storedData = initialData;
    message    = initialMessage;
  }

  function set(uint256 data) {
    emit Set(data);
    storedData = data;
  }

  function get() constant returns (uint256) {
    return storedData;
  }

}
