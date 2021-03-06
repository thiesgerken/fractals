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

'------------------------------------------------------------------------------
' <auto-generated>
'     This code was generated by a tool.
'     Runtime Version:4.0.30319.239
'
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>
'------------------------------------------------------------------------------

Option Strict On
Option Explicit On

Imports System

Namespace My.Resources
    
    'This class was auto-generated by the StronglyTypedResourceBuilder
    'class via a tool like ResGen or Visual Studio.
    'To add or remove a member, edit your .ResX file then rerun ResGen
    'with the /str option, or rebuild your VS project.
    '''<summary>
    '''  A strongly-typed resource class, for looking up localized strings, etc.
    '''</summary>
    <Global.System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0"),  _
     Global.System.Diagnostics.DebuggerNonUserCodeAttribute(),  _
     Global.System.Runtime.CompilerServices.CompilerGeneratedAttribute(),  _
     Global.Microsoft.VisualBasic.HideModuleNameAttribute()>  _
    Friend Module Resources
        
        Private resourceMan As Global.System.Resources.ResourceManager
        
        Private resourceCulture As Global.System.Globalization.CultureInfo
        
        '''<summary>
        '''  Returns the cached ResourceManager instance used by this class.
        '''</summary>
        <Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
        Friend ReadOnly Property ResourceManager() As Global.System.Resources.ResourceManager
            Get
                If Object.ReferenceEquals(resourceMan, Nothing) Then
                    Dim temp As Global.System.Resources.ResourceManager = New Global.System.Resources.ResourceManager("Fractals.Calculators.Resources", GetType(Resources).Assembly)
                    resourceMan = temp
                End If
                Return resourceMan
            End Get
        End Property
        
        '''<summary>
        '''  Overrides the current thread's CurrentUICulture property for all
        '''  resource lookups using this strongly typed resource class.
        '''</summary>
        <Global.System.ComponentModel.EditorBrowsableAttribute(Global.System.ComponentModel.EditorBrowsableState.Advanced)>  _
        Friend Property Culture() As Global.System.Globalization.CultureInfo
            Get
                Return resourceCulture
            End Get
            Set
                resourceCulture = value
            End Set
        End Property
        
        Friend ReadOnly Property Buddhabrot() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("Buddhabrot", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property BuddhabrotKernel() As String
            Get
                Return ResourceManager.GetString("BuddhabrotKernel", resourceCulture)
            End Get
        End Property
        
        Friend ReadOnly Property Julia() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("Julia", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property JuliaKernel() As String
            Get
                Return ResourceManager.GetString("JuliaKernel", resourceCulture)
            End Get
        End Property
        
        Friend ReadOnly Property Multibrot() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("Multibrot", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property MultibrotKernel() As String
            Get
                Return ResourceManager.GetString("MultibrotKernel", resourceCulture)
            End Get
        End Property
        
        Friend ReadOnly Property Newton() As System.Drawing.Bitmap
            Get
                Dim obj As Object = ResourceManager.GetObject("Newton", resourceCulture)
                Return CType(obj,System.Drawing.Bitmap)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property OpenCLFunctions() As String
            Get
                Return ResourceManager.GetString("OpenCLFunctions", resourceCulture)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property PaintKernel() As String
            Get
                Return ResourceManager.GetString("PaintKernel", resourceCulture)
            End Get
        End Property
        
        '''<summary>
        '''  Looks up a localized string similar to // Copyright (C) 2010-2011 Thies Gerken
        '''
        '''// This file is part of Fractals.
        '''
        '''// Fractals is free software: you can redistribute it and/or modify
        '''// it under the terms of the GNU General Public License as published by
        '''// the Free Software Foundation, either version 3 of the License, or
        '''// (at your option) any later version.
        '''
        '''// Fractals is distributed in the hope that it will be useful,
        '''// but WITHOUT ANY WARRANTY; without even the implied warranty of
        '''// MERCHANTABILITY or FITNESS FOR A PARTICULAR  [rest of string was truncated]&quot;;.
        '''</summary>
        Friend ReadOnly Property PowerSeriesKernel() As String
            Get
                Return ResourceManager.GetString("PowerSeriesKernel", resourceCulture)
            End Get
        End Property
    End Module
End Namespace
