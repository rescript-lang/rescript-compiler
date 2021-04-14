let object15 = (destruct
) =>
  Codec.make(
    // encode
    value => {
      let (val1
      ) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
        Field.encode(field6, val6),
        Field.encode(field7, val7),
        Field.encode(field8, val8),
        Field.encode(field9, val9),
        Field.encode(field10, val10),
        Field.encode(field11, val11),
        Field.encode(field12, val12),
        Field.encode(field13, val13),
        Field.encode(field14, val14),
        Field.encode(field15, val15),
      ])
    },
    // decode
    json =>
      json
      ->asObject
      ->Result.flatMap(fieldset =>
        field7->Field.dfmap(fieldset, val7 =>
        field8->Field.dfmap(fieldset, val8 =>
        field9->Field.dfmap(fieldset, val9 =>
        field10->Field.dfmap(fieldset, val10 =>
        field11->Field.dfmap(fieldset, val11 =>
        field12->Field.dfmap(fieldset, val12 =>
        field13->Field.dfmap(fieldset, val13 =>
        field14->Field.dfmap(fieldset, val14 =>
            field15->Field.dfmap(fieldset, val15 => construct((val1, val2, val3,
        val4,
        val5,
        val6,
        val7,
        val8,
        val9,
        val10,
        val11,
        val12,
        val13,
        val14,
        val15,
      )))
          )
        )))))))
      ),
  )