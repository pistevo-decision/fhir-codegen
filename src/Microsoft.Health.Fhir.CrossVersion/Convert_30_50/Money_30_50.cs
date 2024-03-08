// <copyright file="Money.cs" company="Microsoft Corporation">
//     Copyright (c) Microsoft Corporation. All rights reserved.
//     Licensed under the MIT License (MIT). See LICENSE in the repo root for license information.
// </copyright>

using Hl7.Fhir.ElementModel;
using Hl7.Fhir.Model;

namespace Microsoft.Health.Fhir.CrossVersion.Convert_30_50;

public class Money_30_50 : ICrossVersionProcessor<Money>, ICrossVersionExtractor<Money>
{
	private Converter_30_50 _converter;
	internal Money_30_50(Converter_30_50 converter)
	{
		_converter = converter;
	}

	public Money Extract(ISourceNode node)
	{
		Money v = new();
		foreach (ISourceNode child in node.Children())
		{
			Process(child, v);
		}

		return v;
	}

	public void Process(ISourceNode node, Money current)
	{
		switch (node.Name)
		{
			// process inherited elements
			default:
				_converter._element.Process(node, current);
				break;

		}
	}
}