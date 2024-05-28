module vector::vector {
    public fun f() {
        // vector::borrow(&vector[ 1, 2, 3 ], 3)
        vector(vector<vector<u8>>[ vector<u8>[1], 0, 0, 0 ][0]);
    }
}
