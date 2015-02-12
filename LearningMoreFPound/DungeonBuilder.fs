module DungeonBuilder

open System

let wallDungeon size depth (dungeon:DataTypes.cellState[,,]) =
            let adjustedSize = size
            for y = 0 to (depth - 1) do
                for i = 0 to adjustedSize do
                    dungeon.SetValue( new DataTypes.cellState('#', true), [|0; i; y|])
                    dungeon.SetValue( new DataTypes.cellState('#', true), [|adjustedSize; i; y|])
                    dungeon.SetValue( new DataTypes.cellState('#', true), [|i; 0; y;|])
                    dungeon.SetValue( new DataTypes.cellState('#', true), [|i; adjustedSize; y|])
            dungeon


let buildDungeon size depth (playerPosition:int[]) =
    let generator = new Random (DateTime.Now.Millisecond)
    let emptyDungeon = Array3D.create (size + 2) (size + 2) depth (new DataTypes.cellState('.', false))

    let walledDungeon = wallDungeon (size + 1) depth emptyDungeon

    let entrancePosition = playerPosition
    walledDungeon.SetValue(new DataTypes.cellState('^', false), entrancePosition)

    let rec generatePositions numToGenerate dungeonSize currentDepth (positions:List<(int * int)>) (dungeon:DataTypes.cellState[,,]) =
        if numToGenerate > 0 then
            let dimX, dimY = generator.Next(1, dungeonSize), generator.Next(1, dungeonSize)
            if not (List.exists (fun x -> (dimX, dimY) = x) positions) && (dungeon.[dimX, dimY, currentDepth] = new DataTypes.cellState('.', false)) then
                let newPosition = (dimX, dimY)
                let newList = List.Cons(newPosition, positions)
                generatePositions (numToGenerate - 1) dungeonSize currentDepth newList dungeon
            else
                generatePositions numToGenerate dungeonSize currentDepth positions dungeon
        else
            positions

    let addElementsToFloor symbol percent dungeonSize floorDepth (dungeon:DataTypes.cellState[,,]) =
        let elementNumbers = int32 (Math.Floor(percent * float (size * size)))

        let elementPositions = generatePositions elementNumbers dungeonSize floorDepth List.Empty dungeon
        for item in elementPositions do
            dungeon.SetValue(new DataTypes.cellState(symbol, false), [|fst item; snd item; floorDepth|])

    let buildStairs upperFloorDepth lowerFloorDepth dungeonSize (dungeon:DataTypes.cellState[,,]) =
        let downStairsLocation = (generatePositions 1 dungeonSize upperFloorDepth List.Empty dungeon).Head
        dungeon.SetValue(new DataTypes.cellState('▼', false), [|fst downStairsLocation; snd downStairsLocation; upperFloorDepth|])
        dungeon.SetValue(new DataTypes.cellState('^', false), [|fst downStairsLocation; snd downStairsLocation; lowerFloorDepth|])


        
    for level = 0 to (depth - 1) do
        if (level < depth - 1) then
            buildStairs level (level + 1) size walledDungeon

        addElementsToFloor '!' DungeonConfig.wumpusPercent (size + 1) level walledDungeon
        addElementsToFloor 'W' DungeonConfig.weaponPercent (size + 1) level walledDungeon
        addElementsToFloor 'P' DungeonConfig.pitTrapPercent (size + 1) level walledDungeon
        addElementsToFloor '$' DungeonConfig.goldPercent (size + 1) level walledDungeon

    walledDungeon