
let f x = Digest.to_hex @@ Digest.string x 


;; Mt.from_pair_suites __FILE__ @@ Mt.[
    __LOC__, (fun _ -> Eq(f "value", "2063c1608d6e0baf80249c42e2be5804"));
    __LOC__ , (fun _ -> Eq(f  "The quick brown fox jumps over the lazy dog", 
                           "9e107d9d372bb6826bd81d3542a419d6"));
    __LOC__, (fun _ -> Eq(f  "The quick brown fox jumps over the lazy dog.", 
                         "e4d909c290d0fb1ca068ffaddf22cbd0"));
    __LOC__, (fun _ -> Eq(f "", "d41d8cd98f00b204e9800998ecf8427e"));
    __LOC__, (fun _ -> Eq(f ("The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog."  ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog."  ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog."  ), "7065cc36bba1d155fb09f9d02f22e8bf"));
    __LOC__, (fun _ -> Eq(f ("The quick brown fox jumps over the lazy dog." ^ "The quick brown fox jumps over the lazy dog."  ^ "The quick brown fox jumps over the lazy dog."), "b9193d1df4b7a8f0a25ffdd1005c5b2b"))
  ] @ (let ref = [|"d41d8cd98f00b204e9800998ecf8427e"; "0cc175b9c0f1b6a831c399e269772661";
                   "4124bc0a9335c27f086f24ba207a4912"; "47bce5c74f589f4867dbd57e9ca9f808";
                   "74b87337454200d4d33f80c4663dc5e5"; "594f803b380a41396ed63dca39503542";
                   "0b4e7a0e5fe84ad35fb5f95b9ceeac79"; "5d793fc5b00a2348c3fb9ab59e5ca98a";
                   "3dbe00a167653a1aaee01d93e77e730e"; "552e6a97297c53e592208cf97fbb3b60";
                   "e09c80c42fda55f9d992e59ca6b3307d"; "d57f21e6a273781dbf8b7657940f3b03";
                   "45e4812014d83dde5666ebdf5a8ed1ed"; "c162de19c4c3731ca3428769d0cd593d";
                   "451599a5f9afa91a0f2097040a796f3d"; "12f9cf6998d52dbe773b06f848bb3608";
                   "23ca472302f49b3ea5592b146a312da0"; "88e42e96cc71151b6e1938a1699b0a27";
                   "2c60c24e7087e18e45055a33f9a5be91"; "639d76897485360b3147e66e0a8a3d6c";
                   "22d42eb002cefa81e9ad604ea57bc01d"; "bd049f221af82804c5a2826809337c9b";
                   "ff49cfac3968dbce26ebe7d4823e58bd"; "d95dbfee231e34cccb8c04444412ed7d";
                   "40edae4bad0e5bf6d6c2dc5615a86afb"; "a5a8bfa3962f49330227955e24a2e67c";
                   "ae791f19bdf77357ff10bb6b0e97e121"; "aaab9c59a88bf0bdfcb170546c5459d6";
                   "b0f0545856af1a340acdedce23c54b97"; "f7ce3d7d44f3342107d884bfa90c966a";
                   "59e794d45697b360e18ba972bada0123"; "3b0845db57c200be6052466f87b2198a";
                   "5eca9bd3eb07c006cd43ae48dfde7fd3"; "b4f13cb081e412f44e99742cb128a1a5";
                   "4c660346451b8cf91ef50f4634458d41"; "11db24dc3f6c2145701db08625dd6d76";
                   "80dad3aad8584778352c68ab06250327"; "1227fe415e79db47285cb2689c93963f";
                   "8e084f489f1bdf08c39f98ff6447ce6d"; "08b2f2b0864bac1ba1585043362cbec9";
                   "4697843037d962f62a5a429e611e0f5f"; "10c4da18575c092b486f8ab96c01c02f";
                   "af205d729450b663f48b11d839a1c8df"; "0d3f91798fac6ee279ec2485b25f1124";
                   "4c3c7c067634daec9716a80ea886d123"; "d1e358e6e3b707282cdd06e919f7e08c";
                   "8c6ded4f0af86e0a7e301f8a716c4363"; "4c2d8bcb02d982d7cb77f649c0a2dea8";
                   "bdb662f765cd310f2a547cab1cfecef6"; "08ff5f7301d30200ab89169f6afdb7af";
                   "6eb6a030bcce166534b95bc2ab45d9cf"; "1bb77918e5695c944be02c16ae29b25e";
                   "b6fe77c19f0f0f4946c761d62585bfea"; "e9e7e260dce84ffa6e0e7eb5fd9d37fc";
                   "eced9e0b81ef2bba605cbc5e2e76a1d0"; "ef1772b6dff9a122358552954ad0df65";
                   "3b0c8ac703f828b04c6c197006d17218"; "652b906d60af96844ebd21b674f35e93";
                   "dc2f2f2462a0d72358b2f99389458606"; "762fc2665994b217c52c3c2eb7d9f406";
                   "cc7ed669cf88f201c3297c6a91e1d18d"; "cced11f7bbbffea2f718903216643648";
                   "24612f0ce2c9d2cf2b022ef1e027a54f"; "b06521f39153d618550606be297466d5";
                   "014842d480b571495a4a0363793f7367"; "c743a45e0d2e6a95cb859adae0248435";
                   "def5d97e01e1219fb2fc8da6c4d6ba2f"; "92cb737f8687ccb93022fdb411a77cca";
                   "a0d1395c7fb36247bfe2d49376d9d133"; "ab75504250558b788f99d1ebd219abf2";
                   "0f5c6c4e740bfcc08c3c26ccb2673d46"; "cddd19bec7f310d8c87149ef47a1828f";
                   "96b39b8b95e016c79d104d83395b8133"; "f1fc0b14ff8fa674b02344577e23eeb1";
                   "0e8d28a1cafa3ffcff22afd480cce7d8"; "448539ffc17e1e81005b65581855cef4";
                   "61e39aae7c53e6e77db2e4405d9fb157"; "618a426895ee6133a372bebd1129b63e";
                   "046c90690c9e36578b9d4a7e1d249c75"; "aadab38075c43296ee7e12466ebb03e3";
                   "b15af9cdabbaea0516866a33d8fd0f98"; "986e6938ed767a8ae9530eef54bfe5f1";
                   "7ae25a72b71a42ccbc5477fd989cd512"; "98d34e50d4aa7a893cc7919a91acb0e3";
                   "3fc53fc22ea40f1a0afd78fc2cd9aa0f"; "923e37c738b9d7b1526f70b65229cc3d";
                   "b3966b7a08e5d46fd0774b797ba78dc2"; "f50c7286b540bb181db1d6e05a51a296";
                   "4efd6c8826e65a61f82af954d431b59b"; "ef1031e79e7a15a4470a5e98b23781b5";
                   "067876bfd0df0f4c5002780ec85e6f8c"; "789851dfa4c03563e9cef5f7bc050a7e";
                   "baf934720818ee49477e74fc644faa5e"; "9a0ea77ca26d2c121ddcc179edb76308";
                   "20c825561572e33d026f99ddfd999538"; "464c461455c5a927079a13609c20b637";
                   "cf37d42f89b6adb0e1a9e99104501b82"; "d266af45e3d06b70d9f52e2df4344186";
                   "f8b59fa22eb0ba944e2b7aa24d67b681"; "0918d7c2f9062743450a86eae9dde1a3";
                   "36a92cc94a9e0fa21f625f8bfb007adf"; "681d73898dad5685d48b5e8438bc3a66";
                   "337ccef058459c3c16411381778da0c4"; "6ccdfcc742862036ce07583633c5f77e";
                   "ddfa1adc974649dc5b414be86def7457"; "650ebc28ad85f11aa4b63b6ee565b89d";
                   "e4571793bcaba284017eeabd8df85697"; "4fc040d354ad9ba5e4f62862109d3e17";
                   "25814274e02aa7cc03d6314eb703e655"; "11378ecaee0089c840d26352704027e3";
                   "86f950bfcd824d5546da01c40576db31"; "089f243d1e831c5879aa375ee364a06e";
                   "9146ef3527c7cfcc66dc615c3986e391"; "d727cfdfc9ed0347e6917a68b982f7bc";
                   "da8f45e1fdc12deecfe56aeb5288796e"; "29cfcf52d8250a253a535cf7989c7bd2";
                   "0f6eb555b8e3c35411eebe9348594193"; "a922439f963e7e59040e4756992c6f1b";
                   "81f8453cf3f7e5ee5479c777e5a8d80c"; "8a7bd0732ed6a28ce75f6dabc90e1613";
                   "5f61c0ccad4cac44c75ff505e1f1e537"; "f6acfca2d47c87f2b14ca038234d3614";
                   "269fc62c517f3d55c368152addca57e7"; "50587cb16413da779b35508018721647";
                   "5e4a3ecfdaa4636b84a39b6a7be7c047"; "c5339dc2af6bf595580281ffb07353f6";
                   "e51176a47347e167ed0ed766b6de1a0c"; "020406e1d05cdc2aa287641f7ae2cc39";
                   "e510683b3f5ffe4093d021808bc6ff70"; "b325dc1c6f5e7a2b7cf465b9feab7948"|] in

     Ext_array_test.range 0 129
     |> 
     Array.map (fun i -> 
         Printf.sprintf "%d" i, (fun _ -> 
             Mt.Eq ((Digest.to_hex @@ Digest.string @@ String.make i 'a'), ref.(i)))
       )
     |> Array.to_list )

