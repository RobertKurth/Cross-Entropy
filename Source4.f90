                                x1 = Pit_Location(1, Pit_1) + Temporary_Pit_radius(2)
                                y1 = Pit_Location(2, Pit_1) + Temporary_Pit_radius(2)
                                x2 = Pit_Location(1, Pit_2) - Temporary_Pit_radius(2)
                                y2 = Pit_Location(2, Pit_2) - Temporary_Pit_radius(2)
                                Check_Distance = Get_Distance(x1, y1, x2, y2)
                                if(Check_Distance.lt.Combine_Distance) then
                                    Number_of_coalscences(2) = Number_of_coalscences(2) + 1
                                    Temporary_Pit_depth(2) = Pit_depth(Pit_1, 2) + Pit_depth(Pit_2, 2)
                                    Temporary_Pit_radius(1) = Pit_radius(Pit_1, 2) + Pit_radius(Pit_2, 2)
                                    Pit_Location(1, Pit_2) = Two * Cask_Length
                                    Pit_Location(2, Pit_2) = Two * Cask_Length
                                    call Pit_Locations(Next_Random_Number, Number_of_pits, Cask_length, R_Outer, thickness, Weld_thickness, Initial_Pit_radius(1), &
                                        Pit_location, Distance, Pit_1, Pit_2, Coalesced_Pits, Coalesce_distance)
                                endif
                                x1 = Pit_Location(1, Pit_1) + Temporary_Pit_radius(3)
                                y1 = Pit_Location(2, Pit_1) + Temporary_Pit_radius(3)
                                x2 = Pit_Location(1, Pit_2) - Temporary_Pit_radius(3)
                                y2 = Pit_Location(2, Pit_2) - Temporary_Pit_radius(3)
                                Check_Distance = Get_Distance(x1, y1, x2, y2)
                                if(Check_Distance.lt.Combine_Distance) then
                                    Number_of_coalscences(3) = Number_of_coalscences(3) + 1
                                    Temporary_Pit_depth(3) = Pit_depth(Pit_1, 3) + Pit_depth(Pit_2, 3)
                                    Temporary_Pit_radius(1) = Pit_radius(Pit_1, 3) + Pit_radius(Pit_2, 3)
                                    Pit_Location(1, Pit_2) = Two * Cask_Length
                                    Pit_Location(2, Pit_2) = Two * Cask_Length
                                    call Pit_Locations(Next_Random_Number, Number_of_pits, Cask_length, R_Outer, thickness, Weld_thickness, Initial_Pit_radius(1), &
                                        Pit_location, Distance, Pit_1, Pit_2, Coalesced_Pits, Coalesce_distance)
                                endif
                                x1 = Pit_Location(1, Pit_1) + Temporary_Pit_radius(4)
                                y1 = Pit_Location(2, Pit_1) + Temporary_Pit_radius(4)
                                x2 = Pit_Location(1, Pit_2) - Temporary_Pit_radius(4)
                                y2 = Pit_Location(2, Pit_2) - Temporary_Pit_radius(4)
                                Check_Distance = Get_Distance(x1, y1, x2, y2)
                                if(Check_Distance.lt.Combine_Distance) then
                                    Number_of_coalscences(4) = Number_of_coalscences(4) + 1
                                    Temporary_Pit_depth(4) = Pit_depth(Pit_1, 4) + Pit_depth(Pit_2, 4)
                                    Temporary_Pit_radius(4) = Pit_radius(Pit_1, 4) + Pit_radius(Pit_2, 4)
                                    Pit_Location(1, Pit_2) = Two * Cask_Length
                                    Pit_Location(2, Pit_2) = Two * Cask_Length
                                    call Pit_Locations(Next_Random_Number, Number_of_pits, Cask_length, R_Outer, thickness, Weld_thickness, Initial_Pit_radius(1), &
                                        Pit_location, Distance, Pit_1, Pit_2, Coalesced_Pits, Coalesce_distance)
                                endif
