#![cfg(all(feature = "Deref", feature = "DerefMut"))]
#![no_std]

use databend_educe::Educe;

#[test]
#[allow(dead_code)]
fn basic() {
    #[derive(Educe)]
    #[educe(Deref, DerefMut)]
    enum Enum {
        Struct {
            f1: u8,
        },
        Struct2 {
            f1: u8,
            #[educe(Deref, DerefMut)]
            f2: u8,
        },
        Tuple(u8),
        Tuple2(u8, #[educe(Deref, DerefMut)] u8),
    }

    let mut s1 = Enum::Struct {
        f1: 1
    };

    let mut s2 = Enum::Struct2 {
        f1: 1, f2: 2
    };

    let mut t1 = Enum::Tuple(1);

    let mut t2 = Enum::Tuple2(1, 2);

    *s1 += 100;
    *s2 += 100;

    *t1 += 100;
    *t2 += 100;

    assert_eq!(101, *s1);
    assert_eq!(102, *s2);

    assert_eq!(101, *t1);
    assert_eq!(102, *t2);
}
