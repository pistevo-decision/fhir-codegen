// <copyright file="Element.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using Hl7.Fhir.ElementModel;
using Hl7.Fhir.Model;

namespace Microsoft.Health.Fhir.CrossVersion.Convert_30_50;

public class Element_30_50 : ICrossVersionProcessor<Element>
{
	private Converter_30_50 _converter;
	internal Element_30_50(Converter_30_50 converter)
	{
		_converter = converter;
	}

	public void Process(ISourceNode node, Element? current)
	{
        if (current is null)
        {
            return;
        }

		switch (node.Name)
		{
			case "id":
				current.ElementId =node.Text;
				break;

			case "extension":
				current.Extension.Add(_converter._extension.Extract(node));
				break;

		}
	}
}
