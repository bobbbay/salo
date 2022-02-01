# What is Salo?

Salo is a **configuration language for operating systems**. It aims to be a ready-to-go solution for everyone who wants a **reliable, composable OS configuration experience** for themselves and their team.

OS configuration often takes many steps and happens over time. Information about the "how" and "when" a specific property of a deployed machine was changed is spread thin, and is usually kept in the minds of the people who changed said properties. This information is not made easily available to any curious developer, nor is it made clear exactly how these changes were done.

Small, medium-sized, and even large businesses commonly resort to subpar methods of documenting the configuration on their machines: writing them down somewhere, for example. This results in poor expressiability and a spoiled developer experience. Changing just one part of an OS in-production requires time to research and ensure that it doens't break anything else.

That's why we created Salo: An open source solution accessible to everyone, designed to meet a vast majority of needs. Requiring very little effort to be set up, yet highly customizable.

Our solution delivers an **expressive, composable, strongly-typed solution** for **building, deploying, and configuring** operating systems on-the-fly.

## Features

 * Expressive type system (dependent, first-class functions and types)
 * Efficient deployment (leveraging diffs to only deploy what you need)
 * On-the-fly changes (configuring systems as quickly as possible)
 * Powerful history management (view configuration history, and rollback to previous deployments)
 * Staged changes (see exactly what your configuration does, before it's deployed)

Read more about [the features we provide](./FEATURES.md).

## Philosophy

Salo is committed to providing **composable**, **strongly-typed**, **expressive** OS configuration that **focuses on the needs of your development team**.

Read more about [our thoughts on our philosophy](./PHILOSOPHY.md).
