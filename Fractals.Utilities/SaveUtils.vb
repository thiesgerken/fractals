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

Imports System.Runtime.CompilerServices
Imports System.IO
Imports System.Drawing
Imports Fractals.Mathematics
Imports System.Reflection
Imports System.Xml

Public Module SaveUtils
#Region "Methods"
    
    ''' <summary>
    ''' Loads a type from a XmlReader.
    ''' </summary>
    ''' <param name="x">The XmlWriter to write to.</param>
    Public Function LoadType(ByVal x As XElement) As Type
        Dim assemblyName As String = x.Element("Assembly").Value
        Dim typeName As String = x.Element("FullName").Value

        Dim ass = Assembly.Load(assemblyName)
        Dim type As Type = ass.GetType(typeName, True)

        Return type
    End Function
    
    ''' <summary>
    ''' Saves a type to a XmlWriter.
    ''' </summary>
    ''' <param name="type">The type.</param>
    ''' <param name="x">The XmlWriter to write to.</param>
    <Extension()>
    Public Sub WriteType(ByVal x As XmlWriter, ByVal type As Type)
        x.WriteStartElement("Assembly")
        x.WriteValue(type.Assembly.FullName)
        x.WriteEndElement()
        x.WriteStartElement("FullName")
        x.WriteValue(type.FullName)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Saves a color to a XmlWriter.
    ''' </summary>
    ''' <param name="color">The type.</param>
    ''' <param name="x">The XmlWriter to write to.</param>
    <Extension()>
    Public Sub WriteColor(ByVal x As XmlWriter, ByVal color As Color)
        x.WriteStartElement("A")
        x.WriteValue(color.A)
        x.WriteEndElement()

        x.WriteStartElement("R")
        x.WriteValue(color.R)
        x.WriteEndElement()

        x.WriteStartElement("G")
        x.WriteValue(color.G)
        x.WriteEndElement()

        x.WriteStartElement("B")
        x.WriteValue(color.B)
        x.WriteEndElement()
    End Sub

    ''' <summary>
    ''' Loads a color from a XmlReader.
    ''' </summary>
    ''' <param name="x">The XmlWriter to write to.</param>
    Public Function LoadColor(ByVal x As XElement) As Color
        Dim a, r, g, b As Byte

        a = CByte(x.Element("A").Value)
        r = CByte(x.Element("R").Value)
        g = CByte(x.Element("G").Value)
        b = CByte(x.Element("B").Value)

        Return Color.FromArgb(a, r, g, b)
    End Function

    ''' <summary>
    ''' Saves a complex number to a binary stream.
    ''' </summary>
    ''' <param name="writer">The writer to perform the writing on.</param>
    <Extension()> Public Sub Write(ByVal writer As BinaryWriter, ByVal value As ComplexNumber)
        writer.Write(value.RealPart)
        writer.Write(value.ImaginaryPart)
    End Sub

    ''' <summary>
    ''' Loads a complex number from a stream.
    ''' </summary>
    ''' <param name="reader">The reader to read from.</param>
    ''' <returns></returns>
    <Extension()> Public Function ReadComplex(ByVal reader As BinaryReader) As ComplexNumber
        Dim value As New ComplexNumber

        value.RealPart = reader.ReadDouble
        value.ImaginaryPart = reader.ReadDouble

        Return value
    End Function

#End Region
End Module
