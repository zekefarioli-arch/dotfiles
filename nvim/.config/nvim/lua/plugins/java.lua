-- Java / Spring Boot / Gradle on top of LazyVim's java extra (jdtls, java-debug, java-test,
-- Lombok agent from Mason). spring-boot.nvim is not used yet (it needs Neovim 0.12+, now installed).

-- jdtls refuses unknown Gradle wrapper jars. This is the sha256 of gradle-wrapper.jar 9.8.2, checked
-- against https://services.gradle.org/distributions/gradle-9.8.2-wrapper.jar.sha256 (HMCTS starter).
-- A project with another Gradle version needs its own entry here.
-- Known quirk (2026-11-05): jdtls still prints its "gradle wrapper could be malicious" warning in
-- :messages with this set, but the project imports and Spring symbols resolve: harmless noise.
local trusted_gradle_wrappers = {
  { sha256 = "3b8a25775a69158b5ad2b1d17a80a88dc7b40a352a73eb27d06c612a7ce68e98", allowed = true },
}

return {
  {
    "mfussenegger/nvim-jdtls",
    opts = {
      jdtls = {
        settings = {
          java = {
            -- Gradle projects (HMCTS starter): use the project's own wrapper, not a global Gradle
            import = { gradle = { wrapper = { enabled = true } } },
            imports = { gradle = { wrapper = { checksums = trusted_gradle_wrappers } } },
            -- Re-read build.gradle when it changes. The default, "interactive", only asks in a prompt that
            -- is easy to miss in Neovim, so new dependencies stayed unresolved until :JdtUpdateConfig.
            configuration = { updateBuildConfiguration = "automatic" },
            -- Organise imports on save: sorted, unused ones removed
            saveActions = { organizeImports = true },
            completion = {
              importOrder = { "java", "javax", "jakarta", "org", "com", "uk" },
              -- Static members worth completing without an import first (tests, Spring MVC tests)
              favoriteStaticMembers = {
                "org.junit.jupiter.api.Assertions.*",
                "org.assertj.core.api.Assertions.*",
                "org.mockito.Mockito.*",
                "org.mockito.ArgumentMatchers.*",
                "org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*",
                "org.springframework.test.web.servlet.result.MockMvcResultMatchers.*",
              },
            },
            -- Parameter-name hints for literals only; LazyVim keeps inlay hints off (toggle: <leader>uh)
            inlayHints = { parameterNames = { enabled = "literals" } },
            -- "Go to definition" lands on Spring's real source, not a decompiled class
            eclipse = { downloadSources = true },
          },
        },
      },
    },
  },
}
