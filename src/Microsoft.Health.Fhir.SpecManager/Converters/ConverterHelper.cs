﻿// <copyright file="ConverterHelper.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using Microsoft.Health.Fhir.SpecManager.Manager;

namespace Microsoft.Health.Fhir.SpecManager.Converters;

/// <summary>A converter helper.</summary>
public static class ConverterHelper
{
    /// <summary>Get a FHIR Converter for the specified major version.</summary>
    /// <param name="release">The release version.</param>
    /// <returns>An IFhirConverter.</returns>
    public static IFhirConverter ConverterForVersion(FhirVersionInfo.FhirCoreVersion release)
    {
        // create our JSON converter
        switch (release)
        {
            case FhirVersionInfo.FhirCoreVersion.DSTU2:
                return new FromR2();

            case FhirVersionInfo.FhirCoreVersion.STU3:
                return new FromR3();

            case FhirVersionInfo.FhirCoreVersion.R4:
                return new FromR4();

            case FhirVersionInfo.FhirCoreVersion.R4B:
                return new FromR4();

            case FhirVersionInfo.FhirCoreVersion.R5:
                return new FromR5();

            default:
                return new FromR4();
        }
    }

    /// <summary>Converter for version.</summary>
    /// <exception cref="ArgumentNullException">      Thrown when one or more required arguments are
    ///  null.</exception>
    /// <exception cref="ArgumentOutOfRangeException">Thrown when one or more arguments are outside the
    ///  required range.</exception>
    /// <param name="version">The version.</param>
    /// <returns>An IFhirConverter.</returns>
    public static IFhirConverter ConverterForVersion(string version)
    {
        return ConverterForVersion(FhirVersionInfo.MajorReleaseForVersion(version));
    }
}
