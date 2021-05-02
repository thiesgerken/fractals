' Copyright (C) 2010-2012 Thies Gerken

' This file is part of Fractals.

' Fractals is free software: you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation, either version 3 of the License, or
' (at your option) any later version.

' Fractals is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.

' You should have received a copy of the GNU General Public License
' along with Fractals. If not, see <http://www.gnu.org/licenses/>.

Public Enum BandActions
    ''' <summary>
    ''' No action
    ''' </summary>
    None
    ''' <summary>
    ''' Moving
    ''' </summary>
    Move
    ''' <summary>
    ''' Rotation
    ''' </summary>
    Rotate
    ''' <summary>
    ''' Creation of Rubberband
    ''' </summary>
    Size
    ''' <summary>
    ''' Resizing in X Direction
    ''' </summary>
    ReSizeX
    ''' <summary>
    ''' Resizing in Y Direction
    ''' </summary>
    ReSizeY
    ''' <summary>
    ''' Resizing in both X and Y Direction
    ''' </summary>
    ReSizeXY
End Enum
