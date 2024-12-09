//SPDX-License-Identifier: UNLICENSED


pragma solidity 0.8.7;

/*
    Author: Cameron Warnick
    cameronwarnickbusiness@hotmail.com
    github.com/wisecameron
    https://www.linkedin.com/in/cameron-warnick-64a25222a/
    --------------------------------------------------------------------------------------------------
        * Level 0: invalid. 
        * Level 1: Viewer -> Can only gain viewing privileges WITH permissionGate module.
        * Level 2: Auto Operator -> Contract.  With pgate: no perms by default, without pgate: full perms, but cannot 
        add or remove perms under any circumstance.
        * Level 3: operator -> With pgate: No defualt perms, all but permission granting possible.
        Without pgate -> full perms besides granting permissions by default.
        * Level 4: Owner -> with pgate: no default perms, without pgate: full perms
        -- owner can always set permissions
        * Level 5: Autonomous permission granter: Contract, can only grant permissions / add modules.
    --------------------------------------------------------------------------------------------------
    Types: 
    1: uint
    2: int
    3: bool
    4: address
    5: bytes32
    6: static string [0 <= size <= 32]
    7: dynamic string [dynamic]
*/


struct MemberData
{
    uint256 valType;
    uint256 size;
    uint256 bitCount;
}

contract HoneyBadgerDemo
{
    address public owner;

    //bitmap array: |id (4b) | address (20b) | free (8b) |
    uint256[] private moduleData;

    //permissions
    address[] public privilegeList;
    mapping(address => uint256) privileged;

    event ERROR(uint256 errorCode);
    event Flag(string);
    event Diagnostics(uint128 errorCode, uint128 solvable);

    constructor(address _owner) 
    {
        owner = _owner;
        privileged[_owner] = 4;
        privilegeList.push(_owner);
    }

    /*------------------------------------------------------------------------------------------------
                                        Permissions
    ------------------------------------------------------------------------------------------------*/

    /** 
        @notice Removes privilegeList entry AND permissions, since privilegeList 
        entry is bound directly to permissions, one cannot change without the other.
        To just modify a permissionn level, use add_to_privilege_list

        @dev This function will zero out the user's privilegeList entry and 
        remove their permissions.
    */
    function remove_from_privilege_list(address user) external returns (bool) 
    {
        require(privileged[msg.sender] == 5 || privileged[msg.sender] == 4);
        require(privileged[user] != 4);

        uint256 i = 0;
        uint256 l = privilegeList.length;
        bool removed = false;

        for (i; i < l; i++) 
        {
            if (privilegeList[i] == user) 
            {
                privilegeList[i] = address(0);
                privileged[user] = 0;
                removed = true;
            }
        }
        return removed;
    }

    /** 

        @notice This function adds to privilege list array AND sets permission level.  
        Privilege list simply aggregates users with permissions, while the privileged
        stores actual permission levels.  

        @dev This function will try to put the new entry into an existing 
        zero'd out slot before creating a new slot.

        @param level * Level 0: invalid. 
        * Level 1: Viewer -> Can only gain viewing privileges WITH permissionGate module.
        * Level 2: Auto Operator -> Contract.  With pgate: no perms by default, without pgate: full perms, but cannot 
        add or remove perms under any circumstance.
        * Level 3: operator -> With pgate: No defualt perms, all but permission granting possible.
        Without pgate -> full perms besides granting permissions by default.
        * Level 4: Owner -> with pgate: no default perms, without pgate: full perms
        -- owner can always set permissions unless renounced.
        * Level 5: Autonomous permission granter: Contract, can only grant permissions.
    */
    function add_to_privilege_list(address user, uint256 level) external 
    {
        require(privileged[msg.sender] == 4 || privileged[msg.sender] == 5, "insufficient perms!");
        require(level > 0 && level < 6 && level != 4, "invalid target perm level!");

        uint256 i;
        uint256 l = privilegeList.length;

        bool replace = false;
        bool already_included = (privileged[user] != 0);

        //if we are adding a contract, we need to make sure
        //the address is actually a contract address

        uint256 size;
        assembly 
        {
            size := extcodesize(user)
        }
        if (size == 0) 
        {
            if(level == 5 || level == 2)
            {
                revert("Tried to add a user as a contract.");
            }
        }
        if(size > 0 )
        {
            if( level == 1 || level == 3)
            {
                revert("Tried to add contract as a user!");
            }

        }
        
        if (already_included == false) 
        {
            for (i; i < l; i++) 
            {
                if (privilegeList[i] == address(0)) {
                    privilegeList[i] = user;
                    replace = true;
                }
            }
            if (!replace) {
                privilegeList.push(user);
            }
        }

        //update permission level
        privileged[user] = level;
    }

    /** 
        @notice View the privileged level of a user.
        * Level 0: basic user. 
        * Level 1: Viewer -> Can only gain viewing privileges WITH permissionGate module.
        * Level 2: Auto Operator -> Contract.  With pgate: no perms by default, without pgate: full perms, but cannot 
        add or remove perms under any circumstance.
        * Level 3: operator -> With pgate: No defualt perms, all but permission granting possible.
        Without pgate -> full perms besides granting permissions by default.
        * Level 4: Owner -> with pgate: no default perms, without pgate: full perms
        -- owner can always set permissions unless renounced.
        * Level 5: Autonomous permission granter: Contract, can only grant permissions.

        @dev If ret != 0, user is a member of privilegeList[]
    */
    function view_permissions(address user) external view returns (uint256) 
    {
        return (privileged[user]);
    }

    /*------------------------------------------------------------------------------------------------
                                        Owner-Level
    ------------------------------------------------------------------------------------------------*/

    /** 
        @notice For the demo version,  creates the single storage 
        space that will be used.  Strings are not supported in the demo 
        version (types range from [1...5])

        Low-level access: 
        sload(add(1000, mul(1000, sload(storageSpaces.slot)))): memberdata
        sload(add(index, add(1001, mul(1000, sload(storageSpaces.slot)))),packValue): member-specific data

        memberdata is stored at sload(1000) for the simple demo version.

        @param types uint256[] memory This parameter stores the types layout for the storage 
        system, which is stored in the same order. Types are: 1: uint[8...256], 2: int[8...256],
        3: bool[8], 4: address[160], 5: bytes[256]

        @param sizes The size of each element denoted by types.  Bear in mind that the sizes expect to 
        correspond back to a type entry occupying the same relative index.
    */
    function init(
    uint256[] memory types,
    uint256[] memory sizes) 
    external 
    {
        assembly 
        {
            let size
            let i
            let packValue
            let bitCount
            let len
            let scratch

            //validate sender
            mstore(0x0, caller())
            mstore(0x20, privileged.slot)
            scratch := sload(keccak256(0x0, 0x40))
            if iszero(
                or(
                    eq(scratch, 4),
                    eq(scratch, 2)
                )
            )
            {
                mstore(0x0, 0x1)
                revert(0x0, 0x20)
            }

            //prevent double-calling init function
            //not a thing in normal model due to storage spaces
            if iszero(eq(sload(1000), 0))
            {
                mstore(0x0, 0x2)
                revert(0x0, 0x20)
            }

            //used in loop
            len := mload(types)

            //verify sizes and types have equal  size
            if iszero(
                eq(
                    len, mload(sizes)
                )
            )
            {
                mstore(0x0, 0x3)
                revert(0x0, 0x20)
            }

            if iszero(len)
            {
                mstore(0x0, 0x4)
                revert(0x0, 0x20)
            }
            //slot data is stored at 1000
            //[entries (128)][members (128)]

            sstore(1000, len)

            for 
            {} lt(i, len) {i := add(i, 1)} 
            {
                //get the type, size
                packValue := mload(add(add(types, 0x20), mul(0x20, i)))
                size := mload(add(add(sizes, 0x20), mul(0x20, i)))

                //0 < pv < 6
                if or(eq(packValue, 0), gt(packValue, 5))
                {
                    mstore(0x0, 0x2)
                    revert(0x0, 0x20)
                }

                //Case: type === [1,2]
                if or(eq(packValue, 1), eq(packValue, 2)) 
                {
                    //if n & (n - 1) != 0, revert (not pow 2)
                    if iszero(iszero(and(size, sub(size, 1)))) 
                    {
                        mstore(0x0, 0x3)
                        revert(0x0, 0x20)
                    }

                    //verify size
                    if or(lt(size, 8), gt(size, 256)) 
                    {
                        mstore(0x0, 0x4)
                        revert(0x0, 0x20)
                    }
                }

                //if bool set size to 8
                if eq(packValue, 3) 
                {
                    size := 8
                }

                //if address set size to  160
                if eq(packValue, 4) 
                {
                    size := 160
                }

                //if bytes32 set size to 256
                if eq(packValue, 5) 
                {
                    size := 256
                }

                /*
                    OVERFLOW PROTECTION
                    if (bitCount - (256 * (bitCount / 256))) + size > 256, 
                    add bitCount up to nearest 256

                    Why?  That means a packed storage slot would exceed 256 bits.
                */

                //gives number of bits already taken in slot
                scratch := sub(bitCount, mul(256, div(bitCount, 256)))

                //check whether this value fits into the slot
                if gt(add(scratch, size), 256) 
                {
                    //if it does not fit, it will start the next slot
                    bitCount := add(bitCount, sub(256, scratch))
                }

                /*
                    packValue[0...63] = types[i] 
                    packValue[64...128] = sizes[i]
                    packValue[128...256] = bitCount
                */
                packValue := or(
                    or(packValue, shl(64, size)),
                    shl(
                        128,
                        and(bitCount, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
                    )
                )

                //i + 1001
                sstore(add(i, 1001), packValue)

                //the next value is going to start at bitCount[i-1] + size[i-1]
                bitCount := add(bitCount, size)
            }
        }
    }

    /*------------------------------------------------------------------------------------------------
                                            Push
    ------------------------------------------------------------------------------------------------*/

    /**
        @notice Adds one entry to a storage space.

        @dev low-level access: offset = 1000 * storageSpace
        [entries][members] := sload(add(1000, offset))
    */
    function push() external 
    {
        require(privileged[msg.sender] > 1 && privileged[msg.sender] < 5);

        assembly 
        {
            //update the bits on the second level
            let valReference := sload(1000)
            let newValue := shl(128, add(shr(128, valReference), 1))
            sstore(
                1000,
                or(
                    newValue,
                    and(valReference, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
                )
            )
        }
    }

    /**
        @notice Adds "amount" entries to a storage space.

        @dev low-level access: offset = 1000 * storageSpace
        [entries][members] := sload(add(1000, offset))
    */
    function pushMany(uint256 amount) external 
    {
        require(privileged[msg.sender] > 1 && privileged[msg.sender] < 5);

        assembly 
        {
            let valReference := sload(1000)
            let newValue := shl(128, add(shr(128, valReference), amount))
            sstore(
                1000,
                or(
                    newValue,
                    and(valReference, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
                )
            )
        }
    }

    /*------------------------------------------------------------------------------------------------
                                            Put
    ------------------------------------------------------------------------------------------------*/

    /**
        @notice Modify a valid [index < entries] slot corresponding to a 
        [member][entry][storageSpace] combination.

        @dev Low-level access:
        uint256 offset = storageSpace * 1000;
        let member128entry128 := sload(add(1000, offset))

        //data [type, size, bitCount] for the member, storagespace combo
        packValue := sload(add(add(1001, offset), memberIndex))

        //data storage starts at hashed slot (5 billion * storageSpace)
        mstore(
            0x0,
            shl(
                184,
                add(entryIndex, mul(5000000000, storageSpace))
            )
        )

        //add up to the data page -- storage[userIndex][page]
        packValue := sload(add(keccak256(0x0, 0x9), div(bitCount, 256)))
    */
    function put(
        uint256 data,
        uint256 memberIndex,
        uint256 entryIndex
    ) external
    {
        require(privileged[msg.sender] > 1 && privileged[msg.sender] < 5);

        //validate input member and entry indices
        assembly 
        {
            let member128entry128 := sload(1000)

            if or(
                gt(
                    memberIndex,and(member128entry128, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)),
                    gt(entryIndex, shr(128, member128entry128))) 
            {
                mstore(0x0, 0x1)
                revert(0x0, 0x20)
            }
        }

        //verify type and size
        uint256 packValue;

        assembly 
        {
            packValue := sload(add(1001, memberIndex))
        }

        uint256 valType = packValue & 0xFFFFFFFFFFFFFFFF;
        uint256 size = (packValue >> 64) & 0xFFFFFFFFFFFFFFFF;
        uint256 bitCount = (packValue >> 128);

        if (valType == 2) valType = 1;

        //prep data to match any type
        assembly {

            //note: order is speed
            switch valType
            case 1 
            { 
                switch size
                case 8 
                {
                    data := and(data, 0xFF)
                }
                case 256 
                {}
                case 16 
                {
                    data := and(data, 0xFFFF)
                }
                case 32 
                {
                    data := and(data, 0xFFFFFFFF)
                }
                case 64 
                {
                    data := and(data, 0xFFFFFFFFFFFFFFFF)
                }
                case 128 
                {
                    data := and(data, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
                }
                default 
                {
                    mstore(0x0, 0x2)
                    revert(0x0, 0x20)
                }
            }
            case 3 //bool
            { 
                data := and(data, 0x1)
            }
            case 4 //addr
            { 
                data := and(data, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
            }
            case 5 {} //bytes32
            default 
            {
                mstore(0x0, 0x3)
                revert(0x0, 0x20)
            }

            /*get the packed value from the storage system 
            that contains the user's data field*/

            //user-specific storage space
            mstore(0x0, shl(184, entryIndex))

            //add up to the data page -- storage[userIndex][page]
            packValue := sload(add(keccak256(0x0, 0x9), div(bitCount, 256)))

            /* zero out the area that will be replaced */

            //start index is bitCount - (256 * (bitCount / 256))
            //let a := div(bitCount, 256) -> 1042 -> 4.0703125 -> 4
            //a := mul(a, 256) -> 4 * 256 = 1024
            //a := sub(bitCount, a) -> 1042 - 1024 = 18
            //so, our slot is filled until bit 18
            let precedingBits := sub(bitCount, mul(div(bitCount, 256), 256))

            //Given that we set up the bitCount system in init_create / insert_new, 
            //this should NEVER return true.
            if gt(add(precedingBits, size), 256) 
            {
                mstore(0x0, 0x4)
                revert(0x0, 0x20)
            }

            //Create a mapping to zero out our target 
            let mask2 := not(shl(precedingBits, sub(shl(size, 1), 1)))

            //shift data into correct position
            if gt(precedingBits, 0) 
            {
                data := shl(precedingBits, data)
            }

            //Clear the old value and insert our new data
            packValue := or(and(packValue, mask2), data)

            sstore(add(keccak256(0x0, 0x9), div(bitCount, 256)), packValue)
        }
    }

    /*------------------------------------------------------------------------------------------------
                                            Get
    ------------------------------------------------------------------------------------------------*/

    /**
        @notice Retrieve the value corresponding to a valid [member][entry][storageSpace] combination.
        @dev Low-level access:
        uint256 offset = storageSpace * 1000;
        storage space metadata: let member128entry128 := sload(add(1000, offset))

        member-specific metadata: let packValue := sload(add(add(1001, offset), memberIndex))

        slot data:
        mstore(
            0x0,
            shl(
                184,
                add(entryIndex, mul(5000000000, storageSpace))
            )
        )

        //get packed value
        packValue := sload(add(keccak256(0x0, 0x9), div(bitCount, 256)))
    */
    function get(
        uint256 memberIndex,
        uint256 entryIndex
    ) external view returns (uint256) 
    {
        require(privileged[msg.sender] > 0 && privileged[msg.sender] < 5);

        assembly {
            //slot 1000 stores 2 values: number of data members, number of entries in the system
            let member128entry128 := sload(1000)

            //if memberIndex is greater than the max index or entryIndex is gt max entryIndex
            if or(
                gt(
                    memberIndex,
                    and(member128entry128, 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
                ),
                gt(entryIndex, shr(128, member128entry128))
            ) {
                mstore(0x0, 0x1)
                revert(0x0, 0x20)
            }

            //get a description of the value: type [64], size[64], bitCount[64]
            let packValue := sload(add(1001, memberIndex))

            let valType := and(0xFFFFFFFFFFFFFFFF, packValue)
            let size := and(0xFFFFFFFFFFFFFFFF, shr(64, packValue))
            let bitCount := shr(128, packValue)

            //if the type is string or zero, revert
            if or(gt(valType, 5), iszero(valType)) 
            {
                mstore(0x0, 0x2)
                revert(0x0, 0x20)
            }

            /*
                Use bitwise operations to grab the value from the storage page and 
                shift it into place.
            */
            //data storage starts at slot 10,000
            mstore(0x0, shl(184, entryIndex))

            //get packed value
            packValue := sload(add(keccak256(0x0, 0x9), div(bitCount, 256)))

            //isolate value
            let precedingBits := sub(bitCount, mul(div(bitCount, 256), 256))
            packValue := and(shr(precedingBits, packValue), sub(shl(size, 1), 1))

            mstore(0x0, packValue)
            return(0x0, 0x20)
        }
    }

    /*------------------------------------------------------------------------------------------------
                                            View
    ------------------------------------------------------------------------------------------------*/

    /** 
        @notice Returns the number of entries in a storage space.
    */
    function total_entries() external view returns (uint256 entries) 
    {
        assembly {
            entries := shr(128, sload(1000))
        }
    }

    /** 
        @notice Returns the entryData struct for a given entry in
        the storage system.
    */
    function get_member_data(
        uint256 index
    ) public view returns (MemberData memory) 
    {
        MemberData memory e;
        uint256 packValue;


        assembly {
            packValue := sload(add(1001, index))
        }

        e.valType = packValue & 0xFFFFFFFFFFFFFFFF;
        e.size = (packValue >> 64) & 0xFFFFFFFFFFFFFFFF;
        e.bitCount = (packValue >> 128) & 0xFFFFFFFFFFFFFFFF;

        return e;
    }

    /*
        View total member count

        A member corresponds to a field in the storage system.
        For instance, a storage system that tracks stats about dogs.
        A dog's weight would be a member, whereas an entry would be
        an individual dog.
    */
    function get_members() external view returns (uint256) {

        assembly {
            mstore(
                0x0,
                and(
                    sload(1000),
                    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                )
            )
            return(0x0, 0x20)
        }
    }

    /*
        This function is for users to verify that no user address
        has the ability to freely modify storage.  

        returns: addresses with perms, permission levels for each
    */
    function transparency_audit()
        external
        view
        returns (address[] memory, uint256[] memory permissionLevels)
    {
        uint256 l = privilegeList.length;
        uint256 i;

        for (; i < l; i++) {
            permissionLevels[i] = privileged[privilegeList[i]];
        }

        return (privilegeList, permissionLevels);
    }
}