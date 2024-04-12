# dpbuild 0.2.0

* Added back support for LabKey boards. `pinsLabkey` is now required to work with LabKey boards

# dpbuild 0.1.0

## Breaking changes

* dpbuild now requires pins >= v1.2.0. This means that data products will now use the v1 api and older data products are incompatible with dpbuild >= 0.1.0. Quite a few changes under the hood, but users will see minimal changes to the workflow.
* LabKey functionality has been temporarily removed until pins v1 can be extended to support LabKey boards

## Other improvments

* Added a `NEWS.md` file to track changes to the package.
