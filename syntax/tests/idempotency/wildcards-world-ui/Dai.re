type daiContract;
[@bs.send]
external getNonce: (daiContract, Web3.ethAddress) => Js.Promise.t(BN.t) =
  "getNonce";

[@bs.new] [@bs.module "ethers"]
external getContract:
  (Web3.ethAddress, Web3.abi, Web3.web3Library) => daiContract =
  "Contract";

let getDaiContract = (daiAddress, stewardAbi, library, account) => {
  getContract(
    daiAddress,
    stewardAbi,
    ContractUtil.getProviderOrSigner(library, account),
  );
};

[@bs.module "./abi/dai.json"] external daiAbi: Web3.abi = "dai";
